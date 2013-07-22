package com.seattleglassware

import java.io.FileInputStream
import java.io.InputStream
import java.util.Properties
import scala.collection.JavaConversions.seqAsJavaList
import scala.collection.JavaConverters.collectionAsScalaIterableConverter
import scala.language.higherKinds
import scala.util.control.Exception.ultimately
import com.escalatesoft.subcut.inject.BindingModule
import com.escalatesoft.subcut.inject.Injectable
import com.escalatesoft.subcut.inject.bindingIdToString
import com.google.api.client.auth.oauth2.AuthorizationCodeFlow
import com.google.api.client.auth.oauth2.CredentialStore
import com.google.api.client.extensions.appengine.http.UrlFetchTransport
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
import com.google.api.client.http.GenericUrl
import com.google.api.client.json.jackson.JacksonFactory
import com.google.api.client.util.ByteStreams
import com.seattleglassware.BindingIdentifiers.ApplicationName
import com.seattleglassware.BindingIdentifiers.GlassScope
import com.seattleglassware.BindingIdentifiers.OAuthPropertiesFileLocation
import com.seattleglassware.EitherTWithState._
import com.seattleglassware.GlasswareTypes._
import com.seattleglassware.Misc.GenericUrlWithNewScheme
import HttpRequestWrapper.HttpServletRequestWrapper
import JavaInterop.safelyCall
import javax.servlet.FilterChain
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import scalaz._
import scalaz.{ -\/ => -\/ }
import scalaz.{ \/ => \/ }
import scalaz.{ \/- => \/- }
import scalaz.Scalaz._
import java.io.Closeable

object GlasswareTypes {
  val stateTypes = StateGenerator[GlasswareState, EarlyReturn]
  import stateTypes._

  sealed abstract class EarlyReturn {
    lazy val failure = this.liftState
    lazy val exitProcessing = this.liftState
  }
  case class NoSuchParameter(name: String) extends EarlyReturn
  case class NoSuchSessionAttribute(name: String) extends EarlyReturn
  case class WrappedFailure[T](x: T, extraInformation: Option[String] = None) extends EarlyReturn
  case class WrappedNull(extraInformation: Option[String]) extends EarlyReturn
  case class FailedCondition(explanation: String) extends EarlyReturn
  case class FinishedProcessing(explanation: String) extends EarlyReturn
  case class NoAuthenticationToken(reason: EarlyReturn) extends EarlyReturn
  case class ExecuteRedirect(destination: GenericUrl, reason: String) extends EarlyReturn
  case object YieldToNextFilter extends EarlyReturn
  case object PlaceholderReturn extends EarlyReturn

  sealed abstract class Effect
  case class SetResponseContentType(value: String) extends Effect
  case class CopyStreamToOutput(fromStream: InputStream) extends Effect
  case object PlaceholderEffect extends Effect
  case class Comment(s: String) extends Effect
  case class AddMessage(s: String) extends Effect
  case class WriteText(s: String) extends Effect
  case class LogAndIgnore(e: EarlyReturn) extends Effect
  case class SetSessionAttribute(name: String, value: String) extends Effect
  case class CleanupCloseable(x: Closeable) extends Effect

  implicit val partialMonoidForEarlyReturn = new Monoid[EarlyReturn] {
    case object NoOp extends EarlyReturn
    def zero = NoOp
    def append(a: EarlyReturn, b: => EarlyReturn) =
      (a, b) match {
        case (NoOp, b) => b
        case (a, NoOp) => a
        case _         => throw new RuntimeException("""this isnt really a Monoid, I just want to use it on the left side of a \/""")
      }
  }

  implicit class MapErrorsWrapper[T](t: => T) {
    def mapExceptionToLeft(extra: => String = "(no additional information)"): CombinedStateAndFailure[T] =
      {
        try {
          t.right[EarlyReturn]
        } catch {
          case ex: Throwable => WrappedFailure(ex, extra.some).left[T]
        }
      }.liftState

    def mapExceptionOrNullToLeft(extra: => String = "(no additional information)") = safelyCall(t)(
      x => x.right,
      WrappedNull(extra.some).left,
      t => WrappedFailure(t, extra.some).left).liftState
  }

  sealed case class GlasswareState(req: HttpRequestWrapper, effects: List[Effect] = List.empty)

  val effectsThroughGlasswareState = Lens.lensg[GlasswareState, List[Effect]](set = gs => effects => gs.copy(effects = effects),
    get = gs => gs.effects)
  val requestThroughGlasswareState = Lens.lensg[GlasswareState, HttpRequestWrapper](set = gs => req => gs.copy(req = req),
    get = gs => gs.req)
}

trait StatefulParameterOperations extends Injectable {
  import com.seattleglassware.Misc._
  import HttpRequestWrapper._
  import stateTypes._

  implicit val bindingModule: BindingModule

  private val oauthPropertiesFileLocation = inject[String](OAuthPropertiesFileLocation)
  private val urlFetchTransport = inject[UrlFetchTransport]
  private val jacksonFactory = inject[JacksonFactory]
  private val credentialStore = inject[CredentialStore]
  private val glassScope = inject[String](GlassScope)
  private val applicationName = inject[String](ApplicationName)

  def wrapException[T](t: Throwable) = WrappedFailure(t).left[T]
  def wrapMissingParameter[T](s: String) = NoSuchParameter(s).left[T]

  def getParameter(parameterName: String) = for {
    value <- getOptionalParameter(parameterName)
    result <- value.get
      .mapExceptionOrNullToLeft(s"could not get parameter $parameterName")
  } yield result

  def getOptionalParameter(parameterName: String): CombinedStateAndFailure[Option[String]] = for {
    GlasswareState(req, _) <- getGlasswareState
    parameterValue <- req.getParameter(parameterName).right[EarlyReturn].liftState
  } yield parameterValue

  def pushEffect(e: Effect) = for {
    GlasswareState(req, items) <- getGlasswareState
    _ <- put(GlasswareState(req, e :: items)).liftState
  } yield ()

  def pushComment(s: String) = pushEffect(Comment(s))

  def getUserId = getSessionAttribute("userId")

  def getGlasswareState = get[GlasswareState].liftState

  def modifyGlasswareState(s: GlasswareState => GlasswareState) =
    modify[GlasswareState](s).liftState

  def modifyGlasswareEffects(s: List[Effect] => List[Effect]) = for {
    _ <- modifyGlasswareState(x => effectsThroughGlasswareState.mod(s, x))
  } yield ()

  def addGlasswareEffect(e: Effect) = modifyGlasswareEffects(xs => e :: xs)
  def message(s: String) = addGlasswareEffect(AddMessage(s))

  def findSetSessionInState(name: String) = for {
    GlasswareState(_, items) <- getGlasswareState
    result <- items.collectFirst {
      case SetSessionAttribute(n, v) if n == name => v
    }.toRightDisjunction(NoSuchParameter(name)).liftState
  } yield result

  def getSessionAttributeFromRequest(attributeName: String) = for {
    GlasswareState(req, _) <- getGlasswareState
    x <- req.getSessionAttribute[String](attributeName).liftState
  } yield x

  //  def getSessionAttribute(name: String) =
  //    findSetSessionInState(name) orElse getSessionAttributeFromRequest(name)
  def getSessionAttribute(name: String) =
    getSessionAttributeFromRequest(name).orElse(findSetSessionInState(name))

  import HttpRequestWrapper._
  import com.seattleglassware.Misc.GenericUrlWithNewScheme
  import com.seattleglassware.GlasswareTypes.stateTypes._

  def yieldToNextFilterIfPathMatches(fn: PartialFunction[List[String], Boolean], explanation: String) = for {
    pathMatches <- urlPathMatches(fn, explanation)
    redirect <- if (pathMatches) YieldToNextFilter.liftState else noOp
  } yield 1

  val noOp = ().right[EarlyReturn].liftState

  def redirectToHttps = for {
    url <- getGenericUrl
    r <- ExecuteRedirect(url.newScheme("https"), "redirecting to https").liftState
  } yield ()

  def ifAll[T, U](
    predicates: Seq[Function[HttpRequestWrapper, Boolean]],
    trueEffects: HttpRequestWrapper => List[Effect],
    trueResult: T \/ U,
    falseResult: T \/ U) =
    EitherT[StateWithFixedStateType, T, U](State[GlasswareState, T \/ U] {
      case state @ GlasswareState(req, effects) =>
        val alltrue = predicates.forall { _(req) }
        if (alltrue) (GlasswareState(req, trueEffects(req).reverse ++ effects), trueResult) else (state, falseResult)
    })

  import scala.collection.JavaConverters._

  def getGenericUrl = for {
    GlasswareState(req, _) <- getGlasswareState
    url = req.getRequestGenericUrl
  } yield url

  def redirectToNewPath(p: String, reason: String) = for {
    u <- getGenericUrlWithNewPath(p)
    redirectCommand <- ExecuteRedirect(u, reason).exitProcessing
  } yield redirectCommand

  def getGenericUrlWithNewPath(path: String) =
    getGenericUrl.map(_.newRawPath(path))

  def urlSchemeIs(scheme: HttpRequestWrapper.HttpRequestType)(req: HttpRequestWrapper) =
    req.scheme == scheme

  def hostnameMatches(fn: String => Boolean) = urlComponentMatches(_.getHost)(fn(_), "failed to match hostname")

  def urlComponentMatches[T](extractor: GenericUrl => T)(fn: T => Boolean, failureExplanation: String) = for {
    url <- getGenericUrl
    item <- extractor(url)
      .mapExceptionOrNullToLeft(failureExplanation)
  } yield fn(item)

  def urlPathMatches(fn: List[String] => Boolean, explanation: String) =
    urlComponentMatches(_.notNullPathParts)(fn(_), explanation)

  def parameterMatches(parameterName: String, fn: String => Boolean)(req: HttpRequestWrapper) =
    req.getRequestGenericUrl.getAll(parameterName).asScala.map(_.toString).find(fn).isDefined

  def parameterExists(parameterName: String)(req: HttpRequestWrapper) =
    parameterMatches("error", _ != null)(req)

  def newAuthorizationCodeFlow(): CombinedStateAndFailure[AuthorizationCodeFlow] = for {
    authPropertiesStream <- (new FileInputStream(oauthPropertiesFileLocation))
      .mapExceptionOrNullToLeft(s"open auth file oauthPropertiesFileLocation")
    _ <- pushEffect(CleanupCloseable(authPropertiesStream))

    authProperties = new Properties

    _ <- authProperties
      .load(authPropertiesStream)
      .mapExceptionOrNullToLeft("loading properties stream")

    clientId <- authProperties.getProperty("client_id").
      mapExceptionOrNullToLeft("error getting client id")

    clientSecret <- authProperties.getProperty("client_secret").
      mapExceptionOrNullToLeft("error getting client secret")

    result <- (new GoogleAuthorizationCodeFlow.Builder(
      urlFetchTransport,
      jacksonFactory,
      clientId,
      clientSecret,
      Seq(glassScope))
      .setAccessType("offline")
      .setCredentialStore(credentialStore)
      .build())
      .mapExceptionOrNullToLeft("failed to build authorization flow")
  } yield result

  def getCredential = for {
    userId <- getUserId
    credential <- getCredentialForSpecifiedUser(userId)
  } yield credential

  def getCredentialForSpecifiedUser(userId: String) = for {
    authorizationFlow <- newAuthorizationCodeFlow
    credential <- authorizationFlow.loadCredential(userId)
      .mapExceptionOrNullToLeft("error locating credential")
  } yield credential

  def clearUserId(userId: String) = for {
    credential <- getCredential
    deleted <- credentialStore.delete(userId, credential)
      .mapExceptionToLeft("failed to delete credential from store")
  } yield deleted

  def setUserId(uid: String) = pushEffect(SetSessionAttribute(SessionAttributes.USERID, uid))
}

object SessionAttributes {
  val USERID = "userId"
}

trait ServerPlumbing extends StatefulParameterOperations {
  import stateTypes._
  implicit val bindingModule: BindingModule

  def doServerPlumbing[T](req: HttpServletRequest, resp: HttpServletResponse, s: CombinedStateAndFailure[T]) = {
    try {
      val (state, result) = s.run(GlasswareState(req))
      val effects = effectsThroughGlasswareState.get(state)
      executeEffects(effects, req, resp, None, result)
      executeResult(result, req, resp, None)
    } catch {
      case e: Throwable =>
        println(s"snark! $e")
        println("got runtimeexception: ////")
        e.printStackTrace
    }
  }

  def doFilterPlumbing[T](req: ServletRequest, resp: ServletResponse, chain: FilterChain, s: CombinedStateAndFailure[T]): Unit = {
    (req, resp) match {
      case (req: HttpServletRequest, resp: HttpServletResponse) => doHttpFilter(req, resp, chain, s)
      case _ => ()
    }
  }
  private[this] def doHttpFilter[T](req: HttpServletRequest, resp: HttpServletResponse, chain: FilterChain, s: CombinedStateAndFailure[T]): Unit = {
    val (GlasswareState(_, effects), result) = s.run(GlasswareState(req))
    executeEffects(effects, req, resp, chain.some, result)
    executeResult(result, req, resp, chain.some)
  }

  def executeResult[T](result: EarlyReturn \/ T, req: HttpServletRequest, resp: HttpServletResponse, chain: Option[FilterChain]) = result match {
    case -\/(ExecuteRedirect(url, _)) => resp.sendRedirect(url.toString)
    case -\/(YieldToNextFilter)       => chain.get.doFilter(req, resp)
    case _                            => ()
  }

  def executeEffects[T](effects: List[Effect], req: HttpServletRequest, resp: HttpServletResponse, chain: Option[FilterChain], result: EarlyReturn \/ T) = {
    val requrl = req.getRequestURL().toString
    println(s"Run request =====\nearlyReturn: $result\ndoeffects:\n$requrl\n$effects\n========")
    effects.reverse foreach executeEffect(req, resp, chain, result)
  }

  def executeEffect[T](req: HttpServletRequest, resp: HttpServletResponse, chain: Option[FilterChain], result: EarlyReturn \/ T)(e: Effect) = e match {
    case SetResponseContentType(s) =>
      resp.setContentType(s)
    case CopyStreamToOutput(stream) =>
      ultimately(stream.close) {
        ByteStreams.copy(stream, resp.getOutputStream)
      }
    case WriteText(s) => 
      throw new RuntimeException("WriteText not yet implemented")
    case SetSessionAttribute(name, value) =>
      Option(req.getSession) map { _.setAttribute(name, value) }
    case AddMessage(m) =>
      Option(req.getSession) map { _.setAttribute("flash", m) }
    case CleanupCloseable(c) => 
      try {
        c.close
      } catch {
        case e: java.io.IOException => 
      }
    case LogAndIgnore(_) | Comment(_) | PlaceholderEffect => // Do nothing
  }
}

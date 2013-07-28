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
import com.seattleglassware.Misc._
import HttpRequestWrapper.HttpServletRequestWrapper
import JavaInterop.safelyCall
import javax.servlet.FilterChain
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import scala.util._
import scalaz._
import scalaz.{ -\/ => -\/ }
import scalaz.{ \/ => \/ }
import scalaz.{ \/- => \/- }
import scalaz.Scalaz._
import java.io.Closeable
import java.io.InputStreamReader
import java.io.BufferedReader
import com.google.api.services.mirror.model.Notification
import com.seattleglassware.GlasswareTypes.stateTypes._

trait StatefulParameterOperations extends Injectable {
  import com.seattleglassware.Misc._
  import HttpRequestWrapper._

  implicit val bindingModule: BindingModule

  private val oauthPropertiesFileLocation = inject[String](OAuthPropertiesFileLocation)
  private val urlFetchTransport = inject[UrlFetchTransport]
  private val jacksonFactory = inject[JacksonFactory]
  private val credentialStore = inject[CredentialStore]
  private val glassScope = inject[String](GlassScope)
  private val applicationName = inject[String](ApplicationName)

  def wrapException[T](t: Throwable) = WrappedFailure(t).left[T]
  def wrapMissingParameter[T](s: String) = NoSuchParameter(s).left[T]

  implicit class OrElseMessage[T](x: CombinedStateAndFailure[T]) {
    def orElseMessage(s: String) = {
      def prependStateMsg(e: EarlyReturn) = for {
        _ <- addGlasswareEffect(LogAndIgnore(e))
        _ <- addGlasswareEffect(AddMessage(s))
      } yield ()
      x.swap.flatMap(error => prependStateMsg(error).swap).swap
    }
  }

  def unknownCommand(unknownCommand: String) = modifyGlasswareEffects(
    xs => Comment(s"Unknown commmand $unknownCommand") :: AddMessage("I don't know how to do that") :: xs)

  def getInputBufferedReader = for {
    GlasswareState(request, _) <- getGlasswareState
    inputStream <- request.getInputStream
      .mapExceptionToLeft("could not open input stream for notification")
    _ <- pushEffect(CleanupCloseable(inputStream))
    source <- scala.io.Source.fromInputStream(inputStream)
      .mapExceptionToLeft("could not create source from request input stream")
  } yield source

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
    _ <- pushComment("getting credential (userId)")
    userId <- getUserId
    _ <- pushComment("getting credential")
    credential <- getCredentialForSpecifiedUser(userId)
  } yield credential

  def signout = for {
    userId <- getUserId
    credential <- getCredential
    _ <- credentialStore
      .delete(userId, credential)
      .mapExceptionOrNullToLeft("could not delete credential from storage")
    _ <- pushEffect(RemoveSessionAttribute("userId"))
  } yield ()

  def getCredentialForSpecifiedUser(userId: String) = for {
    authorizationFlow <- newAuthorizationCodeFlow
    credential <- authorizationFlow.loadCredential(userId)
      .mapExceptionOrNullToLeft("error locating credential")
  } yield credential

  def getCredentialForNotification(notification: Notification) = for {
    userId <- notification.getUserToken
      .mapExceptionOrNullToLeft("could not get user id from notification")
    credential <- getCredentialForSpecifiedUser(userId)
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
      val writer = resp.getWriter
      writer.append("OK")
      writer.close()
    case SetSessionAttribute(name, value) =>
      Option(req.getSession) map { _.setAttribute(name, value) }
    case RemoveSessionAttribute(name) =>
      Option(req.getSession) map { _.removeAttribute(name) }
    case AddMessage(m) =>
      Option(req.getSession) map { _.setAttribute("flash", m) }
    case CleanupCloseable(c) =>
      try {
        c.close
      } catch {
        case e: java.io.IOException =>
      }
    case SignOut | LogAndIgnore(_) | Comment(_) | PlaceholderEffect => // Do nothing
  }
}

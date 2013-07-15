package com.seattleglassware

import java.io.FileInputStream
import java.io.InputStream
import java.util.Properties
import scala.collection.JavaConversions.seqAsJavaList
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.collectionAsScalaIterableConverter
import scala.util.control.Exception.ultimately
import scala.PartialFunction._
import com.escalatesoft.subcut.inject.BindingModule
import com.escalatesoft.subcut.inject.Injectable
import com.escalatesoft.subcut.inject.bindingIdToString
import com.google.api.client.auth.oauth2.AuthorizationCodeFlow
import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.auth.oauth2.CredentialStore
import com.google.api.client.extensions.appengine.http.UrlFetchTransport
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
import com.google.api.client.http.GenericUrl
import com.google.api.client.json.jackson.JacksonFactory
import com.google.api.client.util.ByteStreams
import com.google.api.services.mirror.Mirror
import com.seattleglassware.BindingIdentifiers._
import JavaInterop.asInstanceOfNotNull
import JavaInterop.safelyCall
import javax.servlet.Filter
import javax.servlet.FilterChain
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import scalaz.Lens
import scalaz.Monoid
import scalaz.EitherT
import scalaz.Bind
import scalaz.Applicative
import scalaz.MonadTrans
import scalaz.Scalaz._
import scalaz.State
import scalaz.\/
import scalaz.-\/
import scalaz.\/-
import com.seattleglassware.EitherTWithState._
import GlasswareTypes._
import com.google.api.client.auth.oauth2.TokenResponse
import com.google.api.client.auth.oauth2.Credential.AccessMethod
import com.google.api.client.http.HttpExecuteInterceptor
import com.google.api.client.http.HttpRequestInitializer
import com.google.api.client.util.Clock
import HttpRequestWrapper._
import com.seattleglassware.Misc._
import com.seattleglassware.GlasswareTypes._
import com.google.api.client.auth.oauth2.CredentialStoreRefreshListener
import com.google.api.services.mirror.model.Contact
import com.google.api.services.mirror.model.Subscription

object GlasswareTypes {
  val stateTypes = StateGenerator[GlasswareState, EarlyReturn]
  import stateTypes._

  sealed abstract class EarlyReturn
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
  case class WriteText(s: String) extends Effect
  case class SetSessionAttribute(name: String, value: String) extends Effect

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

  implicit class CatchExceptionsWrapper[T](t: => T) {
    def catchExceptions(extra: => String = "(no additional information)") = safelyCall(t)(
      x => x.right,
      WrappedNull(extra.some).left,
      t => WrappedFailure(t, extra.some).left)
  }

  implicit class CatchExceptionsTransformerWrapper[T](t: => T) {
    def catchExceptionsT(extra: => String = "(no additional information)") =
      t.catchExceptions(extra).liftState
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

  def getParameter(parameterName: String) =
    State[GlasswareState, EarlyReturn \/ String] {
      state =>
        val parameterValue = requestThroughGlasswareState.get(state).getParameter(parameterName)
        val result = parameterValue.toRightDisjunction(NoSuchParameter(parameterName))
        (state, result)
    }

  def getOptionalParameter(parameterName: String): CombinedStateAndFailure[Option[String]] = for {
    GlasswareState(req, _) <- get[GlasswareState].liftState
    parameterValue <- req.getParameter(parameterName).right[EarlyReturn].liftState
  } yield parameterValue

  def pushEffect(e: Effect): CombinedStateAndFailure[Unit] = for {
    GlasswareState(req, items) <- get[GlasswareState].liftState
    _ <- put(GlasswareState(req, e :: items)).liftState
  } yield 1

  def pushComment(s: String) = pushEffect(Comment(s))

  def getUserId = getSessionAttribute[String]("userId")

  def getSessionAttribute[T](attributeName: String) =
    State[GlasswareState, EarlyReturn \/ T] {
      s => (s, requestThroughGlasswareState.get(s).getSessionAttribute[T](attributeName))
    }

  import HttpRequestWrapper._
  import com.seattleglassware.Misc.GenericUrlWithNewScheme
  import com.seattleglassware.GlasswareTypes.stateTypes._

  def yieldToNextFilterIfPathMatches(fn: PartialFunction[List[String], Boolean], explanation: String) = for {
    pathMatches <- urlPathMatches(fn, explanation)
    redirect <- if (pathMatches) YieldToNextFilter.liftState else noOp
  } yield 1

  def noOp = ().liftState

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
    GlasswareState(req, _) <- get[GlasswareState].liftState
    url = req.getRequestGenericUrl
  } yield url

  def getGenericUrlWithNewPath(path: String) =
    getGenericUrl.map(_.newRawPath(path))

  def urlSchemeIs(scheme: HttpRequestWrapper.HttpRequestType)(req: HttpRequestWrapper) =
    req.scheme == scheme

  def hostnameMatches(fn: String => Boolean) = urlComponentMatches(_.getHost)(fn(_), "failed to match hostname")

  def urlComponentMatches[T](extractor: GenericUrl => T)(fn: T => Boolean, failureExplanation: String) = for {
    url <- getGenericUrl
    item <- extractor(url).catchExceptionsT(failureExplanation)
  } yield fn(item)

  def urlPathMatches(fn: List[String] => Boolean, explanation: String) =
    urlComponentMatches(_.notNullPathParts)(fn(_), explanation)

  def parameterMatches(parameterName: String, fn: String => Boolean)(req: HttpRequestWrapper) =
    req.getRequestGenericUrl.getAll(parameterName).asScala.map(_.toString).find(fn).isDefined

  def parameterExists(parameterName: String)(req: HttpRequestWrapper) =
    parameterMatches("error", _ != null)(req)

  def newAuthorizationCodeFlow(): EarlyReturn \/ AuthorizationCodeFlow = {
    for {
      authPropertiesStream <- (new FileInputStream(oauthPropertiesFileLocation)).
        catchExceptions(s"open auth file oauthPropertiesFileLocation")

      authProperties = new Properties

      _ <- authProperties.load(authPropertiesStream).
        catchExceptions("loading properties stream")

      clientId <- authProperties.getProperty("client_id").
        catchExceptions("error getting client id")

      clientSecret <- authProperties.getProperty("client_secret").
        catchExceptions("error getting client secret")
    } yield {
      new GoogleAuthorizationCodeFlow.Builder(urlFetchTransport, jacksonFactory,
        clientId, clientSecret, Seq(glassScope)).setAccessType("offline")
        .setCredentialStore(credentialStore).build()
    }
  }

  def getCredential(userId: String) = for {
    authorizationFlow <- newAuthorizationCodeFlow
    credential <- authorizationFlow.loadCredential(userId).
      catchExceptions("error locating credential")
  } yield credential

  def clearUserId(userId: String) = for {
    credential <- getCredential(userId)
    deleted <- credentialStore.delete(userId, credential)
      .catchExceptions()
  } yield deleted

  def setUserId(uid: String) = pushEffect(SetSessionAttribute(SessionAttributes.USERID, uid))

  def getAttachmentInputStream(credential: Credential, timelineItemId: String, attachmentId: String) = for {
    mirror <- getMirror(credential)
    attachmentsMetadata <- getAttachmentMetadata(mirror, timelineItemId, attachmentId)
    url <- attachmentsMetadata.getContentUrl
      .catchExceptions("could not get content url for attachment")
    genericUrl <- new GenericUrl(url)
      .catchExceptions(s"could not build genericUrl from [$url]")
    request = mirror.getRequestFactory.buildGetRequest(genericUrl)
    resp <- request.execute
      .catchExceptions("error fetching a mirror request")
    content <- resp.getContent
      .catchExceptions("error getting the content of an attachment")
  } yield content

  def getAttachmentMetadata(mirror: Mirror, timelineItemId: String, attachmentId: String) = {
    for {
      attachments <- mirror.timeline.attachments
        .catchExceptions("failed to get timeline attachments")
      attachmentsRequest <- attachments.get(timelineItemId, attachmentId)
        .catchExceptions("error creating attachments request")
      attachmentsMetadata <- attachmentsRequest.execute
        .catchExceptions("error executing attachments fetch")
    } yield attachmentsMetadata
  }

  def getMirror(credential: Credential) =
    new Mirror.Builder(urlFetchTransport, jacksonFactory, credential)
      .setApplicationName(applicationName)
      .build()
      .right[EarlyReturn]

  def getAttachmentContentType(mirror: Mirror, timelineItemId: String, attachmentId: String): EarlyReturn \/ String = for {
    metadata <- getAttachmentMetadata(mirror, timelineItemId, attachmentId)
    contentType <- metadata.getContentType.catchExceptions("no content type")
  } yield contentType

  def insertContact(credential: Credential, contact: Contact) = {
    getMirror(credential).map {
      _.contacts.insert(contact).execute
    }
  }

  def insertSubscription(credential: Credential, callbackUrl: String, userId: String, collection: String) = for {
    mirror <- getMirror(credential)
    subscription <- (new Subscription)
      .setCollection(collection)
      .setCallbackUrl(callbackUrl)
      .setUserToken(userId)
      .catchExceptions("failed to build subscription")
    insertedSubscription <- mirror.subscriptions
      .insert(subscription)
      .execute
      .catchExceptions("failed to insert subscription.")
  } yield subscription

  def trident[U](x: => U)(returnedValid: U => CombinedStateAndFailure[U],
                          returnedNull: => CombinedStateAndFailure[U],
                          threwException: Throwable => CombinedStateAndFailure[U]) =
    try {
      x match {
        case null => returnedNull
        case x    => returnedValid(x)
      }
    } catch {
      case t: Throwable => threwException(t)
    }

  import scala.language.higherKinds

  def transformLeft[F[+_], A, B](x: => EitherT[F, A, B])(y: A => EitherT[F, A, B])(implicit F: Bind[F]): EitherT[F, A, B] = {
    val g = x.run
    EitherT(F.bind(g) {
      case -\/(l) => y(l).run
      case \/-(_) => g
    })
  }
}

object SessionAttributes {
  val USERID = "userId"
}

trait ServerPlumbing extends StatefulParameterOperations {
  import stateTypes._
  implicit val bindingModule: BindingModule

  def doServerPlumbing[T](req: HttpServletRequest, resp: HttpServletResponse, s: CombinedStateAndFailure[T]) = {
    val (state, result) = s.run(GlasswareState(req))
    val effects = effectsThroughGlasswareState.get(state)
    executeEffects(effects, req, resp, None, result)
    executeResult(result, req, resp, None)
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
    case SetResponseContentType(s) => resp.setContentType(s)
    case CopyStreamToOutput(stream) =>
      ultimately(stream.close) {
        ByteStreams.copy(stream, resp.getOutputStream)
      }
    case WriteText(s) => throw new RuntimeException("WriteText not yet implemented")
    case SetSessionAttribute(name, value) =>
      val session = Option(req.getSession) map { _.setAttribute(name, value) }
    case Comment(_) | PlaceholderEffect => // Do nothing
  }
}

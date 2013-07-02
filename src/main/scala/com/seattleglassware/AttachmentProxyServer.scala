package com.seattleglassware

import java.io.FileInputStream
import java.util.Properties
import scala.collection.JavaConversions.seqAsJavaList
import com.escalatesoft.subcut.inject.BindingId
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
import com.google.api.services.mirror.Mirror
import JavaInterop.asInstanceOfNotNull
import JavaInterop.safelyCall
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import scalaz._
import scalaz.Scalaz._
import BindingIdentifiers._

import StateStuff._
import com.google.api.client.util.ByteStreams

import java.io.{ InputStream, OutputStream }

sealed abstract class EarlyReturn
case class NoSuchParameter(name: String) extends EarlyReturn
case class WrappedFailure[T](x: T, extraInformation: Option[String] = None) extends EarlyReturn
case class WrappedNull(extraInformation: Option[String]) extends EarlyReturn

sealed abstract class Effect
case class SetResponseContentType(value: String) extends Effect
case class CopyStreamToOutput(fromStream: InputStream) extends Effect

object StateStuff {
  val stategen = StateGenerator[GlasswareState, EarlyReturn]
  implicit class CatchExceptionsWrapper[T](t: => T) {
    def catchExceptions(extra: => Option[String] = None) = safelyCall(t)(
      x => x.right,
      WrappedNull(extra).left,
      t => WrappedFailure(t, extra).left)
  }
  case class GlasswareState(req: HttpRequestWrapper, effects: List[Effect])
  val effectsThroughGlasswareState = Lens.lensg[GlasswareState, List[Effect]](set = gs => effects => gs.copy(effects = effects),
    get = gs => gs.effects)
  val requestThroughGlasswareState = Lens.lensg[GlasswareState, HttpRequestWrapper](set = gs => req => gs.copy(req = req),
    get = gs => gs.req)
}

class AttachmentProxyServlet(implicit val bindingModule: BindingModule) extends HttpServlet with StatefulParameterOperations with Injectable {
  import StateStuff.stategen._

  def go = for {
    attachmentId <- getParameter("attachment").liftState
    timelineItemId <- getParameter("timelineItem").liftState
    auth = AuthUtil()
    userid <- getParameter("user_id").liftState
    credential <- auth.getCredential(userid).liftState
    mc = new MirrorClient()
    contentType <- mc.getAttachmentContentType(credential, timelineItemId, attachmentId).liftState
    attachmentInputStream <- mc.getAttachmentInputStream(credential, timelineItemId, attachmentId).liftState
    _ <- pushEffect(SetResponseContentType(contentType)).liftState
    _ <- pushEffect(CopyStreamToOutput(attachmentInputStream)).liftState
  } yield timelineItemId
  
  def fx = go.run(GlasswareState(HttpRequestWrapper.EmptyHttpRequestWrapper, List.empty))
}

trait HttpRequestWrapper {
  import HttpRequestWrapper._

  def getParameter(s: String): Option[String]
  def getScheme: Option[String]
  def getSessionAttribute[T](s: String): EarlyReturn \/ T
  def requestType: HttpRequestType = getScheme match {
    case Some("http")  => Http
    case Some("https") => Https
    case None          => Missing
    case _             => Other
  }
}

object HttpRequestWrapper {
  sealed abstract class HttpRequestType
  case object Http extends HttpRequestType
  case object Https extends HttpRequestType
  case object Missing extends HttpRequestType
  case object Other extends HttpRequestType

  //  abstract sealed class TypedOptionalSessionAttributeResult[T]
  //  case class Success[T](x: T) extends TypedOptionalSessionAttributeResult[T]
  //  case class MissingAttribute[T](attrName: String) extends TypedOptionalSessionAttributeResult[T]
  //  case class IncorrectType[T](attrName: String, result: AnyRef) extends TypedOptionalSessionAttributeResult[T]

  implicit class HttpServletRequestWrapper(r: HttpServletRequest) extends HttpRequestWrapper {
    def getParameter(s: String) = Option(r.getParameter(s))
    def getScheme = Option(r.getScheme()) map { _.toLowerCase }
    def getSessionAttribute[T](s: String): EarlyReturn \/ T =
      safelyCall(r.getSession.getAttribute(s))(
        asInstanceOfNotNull[T](_).right,
        NoSuchParameter(s).left,
        WrappedFailure(_).left)
  }
  
  object EmptyHttpRequestWrapper extends HttpRequestWrapper {
    def getParameter(s: String) = None
    def getScheme = "http".some
    def getSessionAttribute[T](s: String): EarlyReturn \/ T = NoSuchParameter(s).left
  }
}

trait StatefulParameterOperations {
  import HttpRequestWrapper._

  def wrapException[T](t: Throwable) = WrappedFailure(t).left[T]
  def wrapMissingParameter[T](s: String) = NoSuchParameter(s).left[T]

  def getParameter(parameterName: String) =
    State[GlasswareState, EarlyReturn \/ String] {
      case s =>
        val result =
          requestThroughGlasswareState.get(s).getParameter(parameterName).fold(NoSuchParameter(parameterName).left[String])(_.right)
        (s, result)
    }

  //  def setAttribute(parameterName: String, value: String) =
  //    State[GlasswareState, EarlyReturn \/ String] {
  //      case s =>
  //        val newstate = effectsThroughGlasswareState.mod(xs => SetAttribute(parameterName, value) :: xs, s)
  //        (newstate, value.right)
  //    }

  def setResponseContentType(value: String) =
    State[GlasswareState, EarlyReturn \/ String] {
      case s =>
        val newstate = effectsThroughGlasswareState.mod(xs => SetResponseContentType(value) :: xs, s)
        (newstate, value.right)
    }

  def pushEffect(e: Effect) =
    State[GlasswareState, List[Effect]] {
      case s =>
        val newstate = effectsThroughGlasswareState.mod(xs => e :: xs, s)
        (newstate, effectsThroughGlasswareState.get(newstate))
    }

  def getSessionAttribute[T](attributeName: String) =
    State[GlasswareState, EarlyReturn \/ T] {
      case s => (s, requestThroughGlasswareState.get(s).getSessionAttribute[T](attributeName))
    }
}

case class StateGenerator[StateType, FailureType] {
  type StateWithFixedStateType[+A] = State[StateType, A]
  type EitherTWithFailureType[F[+_], A] = EitherT[F, FailureType, A]
  type CombinedStateAndFailure[A] = EitherTWithFailureType[StateWithFixedStateType, A]

  def liftStateA[A](s: StateWithFixedStateType[FailureType \/ A]): CombinedStateAndFailure[A] = EitherT(s)

  implicit class HasLiftFromStateWithFixedStateType[A](s: StateWithFixedStateType[FailureType \/ A]) {
    def liftState: CombinedStateAndFailure[A] = EitherT(s)
  }

  implicit class HasLiftFromEitherOfFailureTypeOrA[A](s: FailureType \/ A) {
    def liftState: CombinedStateAndFailure[A] = EitherT(Applicative[StateWithFixedStateType].point(s))
  }

  implicit class HasLiftFromAnswerType[A](s: A) {
    def liftState: CombinedStateAndFailure[A] = (s.right[FailureType]).liftState
  }

  implicit class HasLiftFromFailureType[A](s: FailureType) {
    def liftState: CombinedStateAndFailure[A] = (s.left[A]).liftState
  }

  implicit class HasLiftFromStateWithoutFailure[A](s: State[StateType, A]) {
    def liftState: CombinedStateAndFailure[A] = sublift(s)
    private[this] def sublift[A](st: StateWithFixedStateType[A]): CombinedStateAndFailure[A] = MonadTrans[EitherTWithFailureType].liftM(st)
  }
}

case class AuthUtil(implicit val bindingModule: BindingModule) extends Injectable {
  import bindingModule._
  import StateStuff._

  val GLASS_SCOPE = "https://www.googleapis.com/auth/glass.timeline " +
    "https://www.googleapis.com/auth/glass.location " +
    "https://www.googleapis.com/auth/userinfo.profile"

  val oauthPropertiesFileLocation = inject[String](OAuthPropertiesFileLocation)
  val urlFetchTransport = inject[UrlFetchTransport]
  val jacksonFactory = inject[JacksonFactory]
  val credentialStore = inject[CredentialStore]

  def newAuthorizationCodeFlow(): EarlyReturn \/ AuthorizationCodeFlow = {
    for {
      authPropertiesStream <- (new FileInputStream(oauthPropertiesFileLocation)).
        catchExceptions(s"open auth file $oauthPropertiesFileLocation".some)

      authProperties = new Properties

      _ <- authProperties.load(authPropertiesStream).
        catchExceptions("loading properties stream".some)

      clientId <- authProperties.getProperty("client_id").
        catchExceptions("error getting client id".some)

      clientSecret <- authProperties.getProperty("client_secret").
        catchExceptions()
    } yield {
      new GoogleAuthorizationCodeFlow.Builder(urlFetchTransport, jacksonFactory,
        clientId, clientSecret, Seq(GLASS_SCOPE)).setAccessType("offline")
        .setCredentialStore(credentialStore).build();
    }
  }

  def getCredential(userId: String) = for {
    authorizationFlow <- newAuthorizationCodeFlow
    credential <- authorizationFlow.loadCredential(userId).
      catchExceptions("error locating credential".some)
  } yield credential

  def clearUserId(userId: String) = for {
    credential <- getCredential(userId)
    deleted <- credentialStore.delete(userId, credential).catchExceptions()
  } yield deleted
}

object BindingIdentifiers {
  object OAuthPropertiesFileLocation extends BindingId
  object ApplicationName extends BindingId
}

class MirrorClient(implicit val bindingModule: BindingModule) extends Injectable {
  import bindingModule._

  val urlFetchTransport = inject[UrlFetchTransport]
  val jacksonFactory = inject[JacksonFactory]
  val applicationName = inject[String](ApplicationName)

  def getMirror(credential: Credential) = {
    new Mirror.Builder(urlFetchTransport, jacksonFactory, credential).
      setApplicationName(applicationName).build().right[EarlyReturn]
  }

  def getAttachmentContentType(credential: Credential, timelineItemId: String, attachmentId: String): EarlyReturn \/ String =
    getAttachmentMetadata(credential, timelineItemId, attachmentId) map { t => t.getContentType }

  def getAttachmentInputStream(credential: Credential, timelineItemId: String, attachmentId: String) = for {
    mirror <- getMirror(credential)
    attachments = mirror.timeline.attachments
    attachmentsMetadata <- attachments.get(timelineItemId, attachmentId).execute.right
    resp <- mirror.getRequestFactory
      .buildGetRequest(new GenericUrl(attachmentsMetadata.getContentUrl)).execute.right
    content <- resp.getContent.right
  } yield content

  def getAttachmentMetadata(credential: Credential, timelineItemId: String, attachmentId: String) = for {
    mirror <- getMirror(credential)
    attachments = mirror.timeline.attachments
    attachmentsMetadata <- attachments.get(timelineItemId, attachmentId).execute.right
  } yield attachmentsMetadata
}
package com.seattleglassware

import javax.servlet.ServletException
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import scalaz._
import scalaz.Scalaz._
import scalaz.State
import scala.reflect.runtime.universe._
import scala.reflect._
import JavaInterop._
import com.google.api.client.auth.oauth2._
import com.escalatesoft.subcut.inject._
import java.io.FileInputStream
import scala.util.control.Exception._
import java.util.Properties
import com.google.api.client.extensions.appengine.http.UrlFetchTransport
import com.google.api.client.json.jackson.JacksonFactory
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
import scalaz.std.iterable
import scala.collection.JavaConversions._

sealed abstract class EarlyReturn
case class NoSuchParameter(name: String) extends EarlyReturn
case class WrappedFailure[T](x: T, extraInformation: Option[String] = None) extends EarlyReturn
case class WrappedNull(extraInformation: Option[String]) extends EarlyReturn

object StateStuff {
  val stategen = StateGenerator[HttpRequestWrapper, EarlyReturn]
  implicit class CatchExceptionsWrapper[T](t: => T) {
    def catchExceptions(extra: => Option[String] = None) = safelyCall(t)(
      x => x.right,
      WrappedNull(extra).left,
      t => WrappedFailure(t, extra).left)
  }
}

class AttachmentProxyServer(implicit val bindingModule: BindingModule) extends HttpServlet with StatefulParameterOperations with Injectable {
  import StateStuff.stategen._

  class AttachmentProxyServlet extends HttpServlet {
    def go: CombinedStateAndFailure[String] = {
      for {
        attachmentId <- getParameter("attachment").liftState
        attachmentId <- getParameter("attachment").liftState
        timelineItemId <- getParameter("timelineItem").liftState
        auth = AuthUtil()
        userid <- getParameter("user_id").liftState
        credential <- auth.getCredential(userid).liftState
      } yield (timelineItemId)
    }
  }
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
}

trait StatefulParameterOperations {
  import HttpRequestWrapper._

  def wrapException[T](t: Throwable) = WrappedFailure(t).left[T]
  def wrapMissingParameter[T](s: String) = NoSuchParameter(s).left[T]

  def getParameter(parameterName: String) =
    State[HttpRequestWrapper, EarlyReturn \/ String] {
      case s =>
        val result =
          s.getParameter(parameterName).fold(NoSuchParameter(parameterName).left[String])(_.right)
        (s, result)
    }

  def getSessionAttribute[T](attributeName: String) =
    State[HttpRequestWrapper, EarlyReturn \/ T] {
      case s => (s, s.getSessionAttribute[T](attributeName))
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

  val oauthPropertiesFileLocation = inject[String]('oauthPropertiesFileLocation)
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

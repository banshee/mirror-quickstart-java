package com.seattleglassware

import javax.servlet.ServletException
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import scalaz._
import scalaz.Scalaz._
import scalaz.State
import scalaz.{ \/ => \/ }
import scala.reflect.runtime.universe._
import scala.reflect._
import JavaInterop._

import com.google.api.client.auth.oauth2._
import com.escalatesoft.subcut.inject.BindingModule

sealed abstract class EarlyReturn
case class NoSuchParameter(name: String) extends EarlyReturn
case class WrappedFailure[T](x: T) extends EarlyReturn

case class InternalState(req: HttpServletRequest) {
  def getParameter(s: String) = Option(req.getParameter(s))
}

object StateStuff {
  val stategen = StateGenerator[InternalState, EarlyReturn]
}

class AttachmentProxyServer extends HttpServlet with StatefulParameterOperations {
  import StateStuff.stategen._

  class AttachmentProxyServlet extends HttpServlet {
    def go = for {
      attachmentId <- getParameter("attachment").liftState
      timelineItemId <- getParameter("timelineItem").liftState
    } yield (attachmentId, timelineItemId)
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

  abstract sealed class TypedOptionalSessionAttributeResult[T]
  case class Success[T](x: T) extends TypedOptionalSessionAttributeResult[T]
  case class MissingAttribute[T](attrName: String) extends TypedOptionalSessionAttributeResult[T]
  case class IncorrectType[T](attrName: String, result: AnyRef) extends TypedOptionalSessionAttributeResult[T]

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

object EitherTWithState {
  case class EarlyExit()

  val fx = State[Int, EarlyExit \/ String] {
    case s if s < 2 => (s + 1, "foo".right)
    case s          => (s + 100, EarlyExit().left)
  }

  type StateA[+A] = State[Int, A]

  val result = for {
    a <- EitherT[StateA, EarlyExit, String](fx)
    _ = println(s"a is $a")
    b <- EitherT[StateA, EarlyExit, String](fx)
    _ = println(s"b is $b")
    c <- EitherT[StateA, EarlyExit, String](fx)
    _ = println(s"c is $c")
  } yield (a, b, c)

  val (finalState, finalResult) = result.run(0)

  println(finalState.toString())
  println(finalResult.toString())
}

object AuthUtil {
  val GLASS_SCOPE = "https://www.googleapis.com/auth/glass.timeline " +
    "https://www.googleapis.com/auth/glass.location " +
    "https://www.googleapis.com/auth/userinfo.profile"

  def newAuthorizationCodeFlow(): AuthorizationCodeFlow = {
    ???
    
  }
}
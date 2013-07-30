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
  case class RemoveSessionAttribute(name: String) extends Effect
  case class CleanupCloseable(x: Closeable) extends Effect
  case object SignOut extends Effect

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

  implicit def convertTryResultToEarlyReturn[T](tryResult: Try[T]): CombinedStateAndFailure[T] =
    tryResult match {
      case scala.util.Success(t) => t.right[EarlyReturn].liftState
      case scala.util.Failure(e) => WrappedFailure(e).left[T].liftState
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

  sealed case class GlasswareState(req: HttpServletRequest, effects: List[Effect] = List.empty)

  val effectsThroughGlasswareState = Lens.lensg[GlasswareState, List[Effect]](set = gs => effects => gs.copy(effects = effects),
    get = gs => gs.effects)
  val requestThroughGlasswareState = Lens.lensg[GlasswareState, HttpServletRequest](set = gs => req => gs.copy(req = req),
    get = gs => gs.req)
}

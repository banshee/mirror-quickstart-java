package com.seattleglassware

import java.io.FileInputStream
import java.io.InputStream
import java.util.Properties

import scala.collection.JavaConversions.seqAsJavaList
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.collectionAsScalaIterableConverter
import scala.util.control.Exception.ultimately

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
import com.seattleglassware.BindingIdentifiers.ApplicationName
import com.seattleglassware.BindingIdentifiers.OAuthPropertiesFileLocation

import JavaInterop.asInstanceOfNotNull
import JavaInterop.safelyCall
import javax.servlet.Filter
import javax.servlet.FilterChain
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
//import scalaz._
import scalaz.Lens
import scalaz.Monoid
import scalaz.EitherT
import scalaz.Applicative
import scalaz.MonadTrans
import scalaz.Scalaz._
import scalaz.State
import scalaz.{\/ => \/}

import com.seattleglassware.StateStuff._

sealed abstract class EarlyReturn
case class NoSuchParameter(name: String) extends EarlyReturn
case class NoSuchSessionAttribute(name: String) extends EarlyReturn
case class WrappedFailure[T](x: T, extraInformation: Option[String] = None) extends EarlyReturn
case class WrappedNull(extraInformation: Option[String]) extends EarlyReturn
case class FailedCondition(explanation: String) extends EarlyReturn
case class FinishedProcessing(explanation: String) extends EarlyReturn

sealed abstract class Effect
case class SetResponseContentType(value: String) extends Effect
case class CopyStreamToOutput(fromStream: InputStream) extends Effect
case class RedirectTo(u: GenericUrl) extends Effect
case object YieldToNextFilter extends Effect

object StateStuff {
  val stategen = StateGenerator[GlasswareState, EarlyReturn]
  implicit class CatchExceptionsWrapper[T](t: => T) {
    def catchExceptions(extra: => String = "(no additional information)") = safelyCall(t)(
      x => x.right,
      WrappedNull(extra.some).left,
      t => WrappedFailure(t, extra.some).left)
  }

  implicit val partialMonoidForEarlyReturn = new Monoid[EarlyReturn] {
    case object NoOp extends EarlyReturn
    def zero = NoOp
    def append(a: EarlyReturn, b: => EarlyReturn) =
      (a, b) match {
        case (NoOp, b) => b
        case (a, NoOp) => a
        case _         => throw new RuntimeException("this isnt really a Monoid")
      }
  }
  case class GlasswareState(req: HttpRequestWrapper, effects: List[Effect] = List.empty)
  val effectsThroughGlasswareState = Lens.lensg[GlasswareState, List[Effect]](set = gs => effects => gs.copy(effects = effects),
    get = gs => gs.effects)
  val requestThroughGlasswareState = Lens.lensg[GlasswareState, HttpRequestWrapper](set = gs => req => gs.copy(req = req),
    get = gs => gs.req)
}

class AttachmentProxyServletSupport(implicit val bindingModule: BindingModule) extends StatefulParameterOperations with Injectable {
  import StateStuff.stategen._

  def attachmentProxyAction: CombinedStateAndFailure[(String, String)] = for {
    attachmentId <- getParameter("attachment").liftState
    timelineItemId <- getParameter("timelineItem").liftState
    userid <- getUserId.liftState

    auth = AuthUtil()
    credential <- auth.getCredential(userid).liftState

    mc = new MirrorClient()
    contentType <- mc.getAttachmentContentType(credential, timelineItemId, attachmentId).liftState
    attachmentInputStream <- mc.getAttachmentInputStream(credential, timelineItemId, attachmentId).liftState

    _ <- pushEffect(SetResponseContentType(contentType)).liftState
    _ <- pushEffect(CopyStreamToOutput(attachmentInputStream)).liftState
  } yield (attachmentId, timelineItemId)
}

class AttachmentProxyServletShim(implicit val bindingModule: BindingModule) extends HttpServlet

class AttachmentProxyServlet extends AttachmentProxyServletShim()(ProjectConfiguration.configuration) with ServerPlumbing {
  val support = new AttachmentProxyServletSupport
  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) = doServer(req, resp, support.attachmentProxyAction)
}

trait HttpRequestWrapper {
  import HttpRequestWrapper._

  def getParameter(s: String): Option[String]
  def getScheme: Option[String]
  def getSessionAttribute[T](s: String): EarlyReturn \/ T
  def scheme: HttpRequestType = getScheme match {
    case Some("http")  => Http
    case Some("https") => Https
    case None          => Missing
    case _             => Other
  }
  def getRequestURI: String
  def getRequestGenericUrl: GenericUrl
  def getHostname: String
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
    def getServerName = Option(r.getServerName)
    def getSessionAttribute[T](s: String): EarlyReturn \/ T =
      safelyCall(r.getSession.getAttribute(s))(
        asInstanceOfNotNull[T](_).right,
        NoSuchSessionAttribute(s).left,
        WrappedFailure(_).left)
    def getRequestURI = r.getRequestURI
    def getRequestGenericUrl = new GenericUrl(getRequestURI)
    def getHostname = getRequestGenericUrl.getHost
  }
}

trait StatefulParameterOperations {
  import HttpRequestWrapper._

  def wrapException[T](t: Throwable) = WrappedFailure(t).left[T]
  def wrapMissingParameter[T](s: String) = NoSuchParameter(s).left[T]

  def getParameter(parameterName: String) =
    State[GlasswareState, EarlyReturn \/ String] {
      state =>
        val result =
          requestThroughGlasswareState.get(state).getParameter(parameterName).fold(NoSuchParameter(parameterName).left[String])(_.right)
        (state, result)
    }

  def pushEffect(e: Effect) =
    State[GlasswareState, Unit] {
      case s =>
        val newstate = effectsThroughGlasswareState.mod(effects => e :: effects, s)
        (newstate, ())
    }

  import StateStuff._

  def getUserId = getSessionAttribute[String]("userId")

  def getSessionAttribute[T](attributeName: String) =
    State[GlasswareState, EarlyReturn \/ T] {
      s => (s, requestThroughGlasswareState.get(s).getSessionAttribute[T](attributeName))
    }

  def executeEffects(effects: List[Effect], resp: HttpServletResponse) = effects.reverse foreach executeEffect(resp)

  def executeEffect(resp: HttpServletResponse)(e: Effect) = e match {
    case SetResponseContentType(s) => resp.setContentType(s)
    case CopyStreamToOutput(stream) =>
      ultimately(stream.close) {
        ByteStreams.copy(stream, resp.getOutputStream)
      }
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
        catchExceptions(s"open auth file $oauthPropertiesFileLocation")

      authProperties = new Properties

      _ <- authProperties.load(authPropertiesStream).
        catchExceptions("loading properties stream")

      clientId <- authProperties.getProperty("client_id").
        catchExceptions("error getting client id")

      clientSecret <- authProperties.getProperty("client_secret").
        catchExceptions("error getting client secret")
    } yield {
      new GoogleAuthorizationCodeFlow.Builder(urlFetchTransport, jacksonFactory,
        clientId, clientSecret, Seq(GLASS_SCOPE)).setAccessType("offline")
        .setCredentialStore(credentialStore).build();
    }
  }

  def getCredential(userId: String) = for {
    authorizationFlow <- newAuthorizationCodeFlow
    credential <- authorizationFlow.loadCredential(userId).
      catchExceptions("error locating credential")
  } yield credential

  def clearUserId(userId: String) = for {
    credential <- getCredential(userId)
    deleted <- credentialStore.delete(userId, credential).catchExceptions()
  } yield deleted
}

class MirrorClient(implicit val bindingModule: BindingModule) extends Injectable {
  import bindingModule._

  def getAttachmentInputStream(credential: Credential, timelineItemId: String, attachmentId: String) = for {
    (mirror, attachments, attachmentsMetadata) <- getAttachmentMetadata(credential, timelineItemId, attachmentId)
    url <- attachmentsMetadata.getContentUrl.catchExceptions("could not get content url for attachment")
    genericUrl <- new GenericUrl(url).catchExceptions(s"could not build genericUrl from [$url]")
    request = mirror.getRequestFactory.buildGetRequest(genericUrl)
    resp <- request.execute.catchExceptions("error fetching a mirror request")
    content <- resp.getContent.catchExceptions("error getting the content of an attachment")
  } yield content

  def getAttachmentMetadata(credential: Credential, timelineItemId: String, attachmentId: String) = for {
    (mirror, attachments) <- getAttachments(credential)
    attachmentsRequest <- attachments.get(timelineItemId, attachmentId).catchExceptions("error creating attachments request")
    attachmentsMetadata <- attachmentsRequest.execute.catchExceptions("error executing attachments fetch")
  } yield (mirror, attachments, attachmentsMetadata)

  def getAttachments(credential: Credential) = for {
    mirror <- getMirror(credential)
    attachments = mirror.timeline.attachments
  } yield (mirror, attachments)

  def getMirror(credential: Credential) =
    new Mirror.Builder(urlFetchTransport, jacksonFactory, credential).
      setApplicationName(applicationName).build().right[EarlyReturn]

  def getAttachmentContentType(credential: Credential, timelineItemId: String, attachmentId: String): EarlyReturn \/ String = for {
    (_, _, metadata) <- getAttachmentMetadata(credential, timelineItemId, attachmentId)
    contentType <- metadata.getContentType.catchExceptions("no content type")
  } yield contentType

  val urlFetchTransport = inject[UrlFetchTransport]
  val jacksonFactory = inject[JacksonFactory]
  val applicationName = inject[String](ApplicationName)
}

trait ServerPlumbing extends StatefulParameterOperations {
  import StateStuff._
  import StateStuff.stategen.CombinedStateAndFailure

  def doServer[T](req: HttpServletRequest, resp: HttpServletResponse, s: CombinedStateAndFailure[T]) = {
    val (state, result) = s.run(GlasswareState(req))
    val effects = effectsThroughGlasswareState.get(state)
    executeEffects(effects, resp)
  }

  def doFilter[T](req: ServletRequest, resp: ServletResponse, chain: FilterChain, s: CombinedStateAndFailure[T]): Unit =
    (req, resp) match {
      case (req: HttpServletRequest, resp: HttpServletResponse) => doHttpFilter(req, resp, chain, s)
      case _ => ()
    }

  def doHttpFilter[T](req: HttpServletRequest, resp: HttpServletResponse, chain: FilterChain, s: CombinedStateAndFailure[T]): Unit = {
    val (state, result) = s.run(GlasswareState(req))
    val effects = effectsThroughGlasswareState.get(state)
    executeEffects(effects, resp)
  }
}

class AuthFilterSupport(implicit val bindingModule: BindingModule) extends StatefulParameterOperations with Injectable {
  import bindingModule._
  import HttpRequestWrapper._

  import StateStuff.stategen._

  def appspotHttpsCheck = ifAll(
    predicates = List(hostnameMatches(_.contains("appspot.com")), urlSchemeIs(Http)),
    trueEffects = redirectToHttps,
    trueResult = FinishedProcessing("running on appspot requires https").left,
    falseResult = ().right)

  def middleOfAuthFlowCheck = yieldToNextFilterIfFirstElementOfPathMatches("oauth2callback", "Skipping auth check during auth flow")
  def isRobot = yieldToNextFilterIfFirstElementOfPathMatches("notify", "Skipping auth check for notify servlet")

  def yieldToNextFilterIfFirstElementOfPathMatches(p: String, explanation: String) =
    yieldToNextFilterIfPathMatches({ case h :: t => p == h }, explanation)

  def yieldToNextFilterIfPathMatches(fn: PartialFunction[List[String], Boolean], explanation: String) = ifAll(
    predicates = List(urlPathMatches(fn)),
    trueEffects = _ => List(YieldToNextFilter),
    trueResult = FinishedProcessing(explanation).left,
    falseResult = ().right)

  //
  //          LOG.fine("Checking to see if anyone is logged in");
  //      if (AuthUtil.getUserId(httpRequest) == null
  //          || AuthUtil.getCredential(AuthUtil.getUserId(httpRequest)) == null
  //          || AuthUtil.getCredential(AuthUtil.getUserId(httpRequest)).getAccessToken() == null) {
  //        // redirect to auth flow
  //        httpResponse.sendRedirect(WebUtil.buildUrl(httpRequest, "/oauth2callback"));
  //        return;
  //      }
  //

  def redirectToHttps(req: HttpRequestWrapper) = List(RedirectTo(req.getRequestGenericUrl.newScheme("https")))

  def authFilterAction: CombinedStateAndFailure[String] = for {
    _ <- appspotHttpsCheck.liftState
    _ <- middleOfAuthFlowCheck.liftState
    _ <- isRobot.liftState
  } yield "foo"

  def ifAll[T, U](predicates: Seq[Function[HttpRequestWrapper, Boolean]], trueEffects: HttpRequestWrapper => List[Effect], trueResult: T \/ U, falseResult: T \/ U) =
    State[GlasswareState, T \/ U] {
      case state @ GlasswareState(req, effects) =>
        val alltrue = predicates.forall { _(req) }
        if (alltrue) (GlasswareState(req, trueEffects(req).reverse ++ effects), trueResult) else (state, falseResult)
    }

  implicit class GenericUrlWithNewScheme(u: GenericUrl) {
    def newScheme(scheme: String) = {
      val result = u.clone
      result.setScheme(scheme)
      result
    }
  }

  import scala.collection.JavaConverters._

  def urlSchemeIs(scheme: HttpRequestWrapper.HttpRequestType)(req: HttpRequestWrapper) =
    req.scheme == scheme

  def hostnameMatches(fn: String => Boolean)(req: HttpRequestWrapper) =
    fn(req.getHostname)

  def urlPathMatches(fn: PartialFunction[List[String], Boolean])(req: HttpRequestWrapper) =
    fn(req.getRequestGenericUrl.getPathParts.asScala.toList)

  def parameterMatches(parameterName: String, fn: String => Boolean)(req: HttpRequestWrapper) =
    req.getRequestGenericUrl.getAll(parameterName).asScala.map(_.toString).find(fn).isDefined

}

abstract class AuthFilterShim(implicit val bindingModule: BindingModule) extends Filter
class AuthFilter extends AuthFilterShim()(ProjectConfiguration.configuration) with ServerPlumbing with Injectable {
  val support = new AuthFilterSupport
  override def doFilter(req: ServletRequest, resp: ServletResponse, chain: FilterChain) = doFilter(req, resp, chain, support.authFilterAction)
  def destroy(): Unit = ??? 
  def init(x$1: javax.servlet.FilterConfig): Unit = ???
}


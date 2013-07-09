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
import scalaz.Applicative
import scalaz.MonadTrans
import scalaz.Scalaz._
import scalaz.State
import scalaz.\/
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
import com.seattleglassware.HttpSupport._
import com.google.api.client.auth.oauth2.CredentialStoreRefreshListener

object GlasswareTypes {
  val stateTypes = StateGenerator[GlasswareState, EarlyReturn]

  sealed abstract class EarlyReturn
  case class NoSuchParameter(name: String) extends EarlyReturn
  case class NoSuchSessionAttribute(name: String) extends EarlyReturn
  case class WrappedFailure[T](x: T, extraInformation: Option[String] = None) extends EarlyReturn
  case class WrappedNull(extraInformation: Option[String]) extends EarlyReturn
  case class FailedCondition(explanation: String) extends EarlyReturn
  case class FinishedProcessing(explanation: String) extends EarlyReturn
  case class NoAuthenticationToken(reason: EarlyReturn) extends EarlyReturn
  case class ExecuteRedirect(reason: EarlyReturn) extends EarlyReturn

  sealed abstract class Effect
  case class SetResponseContentType(value: String) extends Effect
  case class CopyStreamToOutput(fromStream: InputStream) extends Effect
  case class SetRedirectTo(u: GenericUrl, explanation: String) extends Effect
  case object YieldToNextFilter extends Effect
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

  sealed case class GlasswareState(req: HttpRequestWrapper, effects: List[Effect] = List.empty)

  val effectsThroughGlasswareState = Lens.lensg[GlasswareState, List[Effect]](set = gs => effects => gs.copy(effects = effects),
    get = gs => gs.effects)
  val requestThroughGlasswareState = Lens.lensg[GlasswareState, HttpRequestWrapper](set = gs => req => gs.copy(req = req),
    get = gs => gs.req)
}

trait StatefulParameterOperations {
  import com.seattleglassware.Misc._
  import HttpRequestWrapper._

  def wrapException[T](t: Throwable) = WrappedFailure(t).left[T]
  def wrapMissingParameter[T](s: String) = NoSuchParameter(s).left[T]

  def getParameter(parameterName: String) =
    State[GlasswareState, EarlyReturn \/ String] {
      state =>
        val parameterValue = requestThroughGlasswareState.get(state).getParameter(parameterName)
        val result = parameterValue.toRightDisjunction(NoSuchParameter(parameterName))
        (state, result)
    }

  def getOptionalParameter(parameterName: String) =
    State[GlasswareState, EarlyReturn \/ Option[String]] {
      state =>
        val parameterValue = requestThroughGlasswareState.get(state).getParameter(parameterName)
        (state, parameterValue.right)
    }

  def pushEffect(e: Effect) =
    State[GlasswareState, List[Effect]] {
      case s =>
        val newstate = effectsThroughGlasswareState.mod(effects => e :: effects, s)
        (newstate, newstate.effects)
    }

  def pushComment(s: String) = pushEffect(Comment(s))

  def getUserId = getSessionAttribute[String]("userId")

  def getSessionAttribute[T](attributeName: String) =
    State[GlasswareState, EarlyReturn \/ T] {
      s => (s, requestThroughGlasswareState.get(s).getSessionAttribute[T](attributeName))
    }

  val isRedirectLocation: Effect =?> GenericUrl = {
    case SetRedirectTo(x, _) => x
  }

  def executeEffects[T](effects: List[Effect], req: HttpServletRequest, resp: HttpServletResponse, chain: Option[FilterChain], earlyReturn: Option[EarlyReturn]) = {
    val requrl = req.getRequestURL().toString
    val (specialeffects, normaleffects) = effects.partition(isRedirectLocation.isDefinedAt(_))
    println(s"Run request =====\nearlyReturn: $earlyReturn\ndoeffects:\n$requrl\n$normaleffects\n$specialeffects\n========")
    normaleffects.reverse foreach executeEffect(req, resp, chain, earlyReturn)
    executeSpecialEffects(specialeffects, req, resp, chain, earlyReturn)
  }

  def executeSpecialEffects[T](effects: List[Effect], req: HttpServletRequest, resp: HttpServletResponse, chain: Option[FilterChain], earlyReturn: Option[EarlyReturn]) = {
    // If we get a ExecuteRedirect as the early return, 
    // use the most recent redirect location as the address
    earlyReturn collectFirst {
      case ExecuteRedirect(_) =>
        val url = effects collectFirst isRedirectLocation
        url foreach {
          x =>
            println("Redirecting to $x")
            resp.sendRedirect(x.toString)
        }
    }
  }

  def executeEffect(req: HttpServletRequest, resp: HttpServletResponse, chain: Option[FilterChain], earlyReturn: Option[EarlyReturn])(e: Effect) = (earlyReturn, e) match {
    case (_, SetResponseContentType(s)) => resp.setContentType(s)
    case (_, CopyStreamToOutput(stream)) =>
      ultimately(stream.close) {
        ByteStreams.copy(stream, resp.getOutputStream)
      }
    case (Some(ExecuteRedirect(_)), SetRedirectTo(url, _)) => resp.sendRedirect(url.toString)
    case (_, SetRedirectTo(url, _))                        => ()
    case (_, YieldToNextFilter)                            => chain.get.doFilter(req, resp)
    case (_, Comment(_)) | (_, PlaceholderEffect)          => // Do nothing
  }
}

case class AuthUtil(implicit val bindingModule: BindingModule) extends Injectable with StatefulParameterOperations {
  import bindingModule._
  import EitherTWithState._

  val GLASS_SCOPE = "https://www.googleapis.com/auth/glass.timeline " +
    "https://www.googleapis.com/auth/glass.location " +
    "https://www.googleapis.com/auth/userinfo.profile"

  val oauthPropertiesFileLocation = inject[String](OAuthPropertiesFileLocation)
  val urlFetchTransport = inject[UrlFetchTransport]
  val jacksonFactory = inject[JacksonFactory]
  val credentialStore = inject[CredentialStore]
//  val credentialMethod = inject[AccessMethod]
//  val tokenServerEncodedUrl = inject[String](TokenServerEncodedUrl)
//  val clientAuthentication = inject[HttpExecuteInterceptor]
//  val requestInitializer = inject[HttpRequestInitializer]
//  val clock = inject[Clock](AuthenticationClock)

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

  def setUserId(uid: String) = for {
    _ <- pushEffect(SetSessionAttribute(SessionAttributes.USERID, uid))
  } yield uid
}

object SessionAttributes {
  val USERID = "userId"
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
  import stateTypes._

  def doServerPlumbing[T](req: HttpServletRequest, resp: HttpServletResponse, s: CombinedStateAndFailure[T]) = {
    val (state, result) = s.run(GlasswareState(req))
    val effects = effectsThroughGlasswareState.get(state)
    executeEffects(effects, req, resp, None, result.swap.toOption)
  }

  def doFilterPlumbing[T](req: ServletRequest, resp: ServletResponse, chain: FilterChain, s: CombinedStateAndFailure[T]): Unit =
    (req, resp) match {
      case (req: HttpServletRequest, resp: HttpServletResponse) => doHttpFilter(req, resp, chain, s)
      case _ => ()
    }

  private[this] def doHttpFilter[T](req: HttpServletRequest, resp: HttpServletResponse, chain: FilterChain, s: CombinedStateAndFailure[T]): Unit = {
    val (GlasswareState(_, effects), result) = s.run(GlasswareState(req))
    executeEffects(effects, req, resp, chain.some, result.swap.toOption)
  }
}

object HttpSupport {
  import HttpRequestWrapper._
  import com.seattleglassware.Misc.GenericUrlWithNewScheme
  import com.seattleglassware.GlasswareTypes.stateTypes._

  def yieldToNextFilterIfPathMatches(fn: PartialFunction[List[String], Boolean], explanation: String) = ifAll(
    predicates = List(urlPathMatches(fn)),
    trueEffects = _ => List(YieldToNextFilter),
    trueResult = FinishedProcessing(explanation).left,
    falseResult = ().right)

  def redirectToHttps(req: HttpRequestWrapper) = List(SetRedirectTo(req.getRequestGenericUrl.newScheme("https"), "https required"))

  def ifAll[T, U](predicates: Seq[Function[HttpRequestWrapper, Boolean]], trueEffects: HttpRequestWrapper => List[Effect], trueResult: T \/ U, falseResult: T \/ U) =
    State[GlasswareState, T \/ U] {
      case state @ GlasswareState(req, effects) =>
        val alltrue = predicates.forall { _(req) }
        if (alltrue) (GlasswareState(req, trueEffects(req).reverse ++ effects), trueResult) else (state, falseResult)
    }

  import scala.collection.JavaConverters._

  def getGenericUrl = for {
    GlasswareState(req, _) <- get[GlasswareState].liftState
    url = req.getRequestGenericUrl
  } yield url

  def urlSchemeIs(scheme: HttpRequestWrapper.HttpRequestType)(req: HttpRequestWrapper) =
    req.scheme == scheme

  def hostnameMatches(fn: String => Boolean)(req: HttpRequestWrapper) =
    fn(req.getHostname)

  def urlPathMatches(fn: PartialFunction[List[String], Boolean])(req: HttpRequestWrapper) = {
    val items = req.getRequestGenericUrl.getPathParts.asScala.filter { s => s != null && s.length > 0 }.toList
    fn.lift(items) | false
  }

  def parameterMatches(parameterName: String, fn: String => Boolean)(req: HttpRequestWrapper) =
    req.getRequestGenericUrl.getAll(parameterName).asScala.map(_.toString).find(fn).isDefined

  def parameterExists(parameterName: String)(req: HttpRequestWrapper) =
    parameterMatches("error", _ != null)(req)
}

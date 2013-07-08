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

class AuthFilterSupport(implicit val bindingModule: BindingModule) extends StatefulParameterOperations with Injectable {
  import stateTypes._
  
  import bindingModule._
  import HttpRequestWrapper._
  import com.seattleglassware.Misc._

  def appspotHttpsCheck = ifAll(
    predicates = List(hostnameMatches(_.contains("appspot.com")), urlSchemeIs(Http)),
    trueEffects = redirectToHttps,
    trueResult = ExecuteRedirect.left,
    falseResult = ().right)

  def middleOfAuthFlowCheck = yieldToNextFilterIfFirstElementOfPathMatches("oauth2callback", "Skipping auth check during auth flow")
  def isRobotCheck = yieldToNextFilterIfFirstElementOfPathMatches("notify", "Skipping auth check for notify servlet")

  def yieldToNextFilterIfFirstElementOfPathMatches(p: String, explanation: String) =
    yieldToNextFilterIfPathMatches({
      case h :: t => p == h
    }, explanation)

  val auth = AuthUtil()

  def setRedirectLocation(path: String, reason: String) = for {
    GlasswareState(req, _) <- get[GlasswareState].liftState
    url = req.getRequestGenericUrl.newRawPath(path)
    _ <- pushEffect(SetRedirectTo(url, reason)).liftState
  } yield url

  def getAccessToken = {
    val computation = for {
      redirectlocation <- setRedirectLocation("/oauth2callback", "failed to get auth token")
      userId <- getUserId.liftState
      credential <- auth.getCredential(userId).liftState
      accessToken <- safelyCall(credential.getAccessToken)(
        returnedValid = _.right[EarlyReturn],
        returnedNull = FailedCondition("no access token").left,
        threwException = t => WrappedFailure(t).left).liftState
    } yield accessToken
    // Turn all failures into an ExecuteRedirect
    computation.fold(left => ExecuteRedirect(reason = left).left[String],
      right => right.right[EarlyReturn])
  }

  def yieldToNextFilterIfPathMatches(fn: PartialFunction[List[String], Boolean], explanation: String) = ifAll(
    predicates = List(urlPathMatches(fn)),
    trueEffects = _ => List(YieldToNextFilter),
    trueResult = FinishedProcessing(explanation).left,
    falseResult = ().right)

  def redirectToHttps(req: HttpRequestWrapper) = List(SetRedirectTo(req.getRequestGenericUrl.newScheme("https"), "https required"))

  def authenticationCheck: CombinedStateAndFailure[String] = for {
    _ <- pushComment("start authentication check").liftState
    _ <- appspotHttpsCheck.liftState
    _ <- middleOfAuthFlowCheck.liftState
    _ <- isRobotCheck.liftState
    token <- getAccessToken.liftState
    _ <- pushEffect(YieldToNextFilter).liftState
    _ <- pushComment("finish authentication check").liftState
  } yield token

  def ifAll[T, U](predicates: Seq[Function[HttpRequestWrapper, Boolean]], trueEffects: HttpRequestWrapper => List[Effect], trueResult: T \/ U, falseResult: T \/ U) =
    State[GlasswareState, T \/ U] {
      case state @ GlasswareState(req, effects) =>
        val alltrue = predicates.forall { _(req) }
        if (alltrue) (GlasswareState(req, trueEffects(req).reverse ++ effects), trueResult) else (state, falseResult)
    }

  import scala.collection.JavaConverters._

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
}

class AuthFilter extends FilterInjectionShim()(ProjectConfiguration.configuration) with NonInitializedFilter {
  val filterImplementation = (new AuthFilterSupport).authenticationCheck
}

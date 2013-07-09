package com.seattleglassware

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.collectionAsScalaIterableConverter
import com.escalatesoft.subcut.inject.BindingModule
import com.escalatesoft.subcut.inject.Injectable
import com.seattleglassware.Misc.GenericUrlWithNewScheme
import GlasswareTypes.EarlyReturn
import GlasswareTypes.Effect
import GlasswareTypes.ExecuteRedirect
import GlasswareTypes.FailedCondition
import GlasswareTypes.FinishedProcessing
import GlasswareTypes.GlasswareState
import GlasswareTypes.SetRedirectTo
import GlasswareTypes.WrappedFailure
import GlasswareTypes.YieldToNextFilter
import GlasswareTypes.partialMonoidForEarlyReturn
import GlasswareTypes.stateTypes.CombinedStateAndFailure
import GlasswareTypes.stateTypes.HasLiftFromEitherOfFailureTypeOrA
import GlasswareTypes.stateTypes.HasLiftFromStateWithFixedStateType
import GlasswareTypes.stateTypes.HasLiftFromStateWithoutFailure
import HttpRequestWrapper.Http
import JavaInterop.safelyCall
import scalaz.Scalaz._
import scalaz.State
import scalaz.{ \/ => \/ }
import HttpRequestWrapper._
import com.seattleglassware.Misc._
import com.seattleglassware.GlasswareTypes.stateTypes._
import com.google.api.client.googleapis.auth.oauth2.GoogleTokenResponse
import com.google.api.client.auth.oauth2.TokenResponse

class AuthFilterSupport(implicit val bindingModule: BindingModule) extends StatefulParameterOperations with Injectable {
  import com.seattleglassware.GlasswareTypes.stateTypes._

  import bindingModule._

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
    computation.fold(
      failureReason => ExecuteRedirect(reason = failureReason).left[String],
      success => success.right[EarlyReturn])
  }

  def authenticationCheck: CombinedStateAndFailure[String] = for {
    _ <- pushComment("start authentication check").liftState
    _ <- appspotHttpsCheck.liftState
    _ <- middleOfAuthFlowCheck.liftState
    _ <- isRobotCheck.liftState
    token <- getAccessToken.liftState
    _ <- pushEffect(YieldToNextFilter).liftState
    _ <- pushComment("finish authentication check").liftState
  } yield token

}

class AuthFilter extends FilterInjectionShim()(ProjectConfiguration.configuration) with NonInitializedFilter {
  val filterImplementation = (new AuthFilterSupport).authenticationCheck
}

// ----- 

class AuthServletSupport(implicit val bindingModule: BindingModule) extends StatefulParameterOperations with Injectable {
  import com.seattleglassware.GlasswareTypes.stateTypes._

  import bindingModule._
  import HttpRequestWrapper._
  import com.seattleglassware.Misc._
  import com.seattleglassware.GlasswareTypes._

  def finishOAuth2Dance(code: String): CombinedStateAndFailure[Int] = for {
    _ <- pushComment("finishing OAuth2 dance").liftState
    auth = new AuthUtil()
    redirectUri <- getGenericUrl.liftState
    flow <- auth.newAuthorizationCodeFlow.liftState
    url <- getGenericUrl.map(_.newRawPath("/oauth2callback").toString)
    tokenResponse <- flow.newTokenRequest(code)
      .setRedirectUri(url)
      .execute
      .catchExceptions("newtokenrequest failed")
      .liftState
    userId <- tokenResponse.asInstanceOf[GoogleTokenResponse]
      .parseIdToken
      .getPayload
      .getUserId
      .catchExceptions("extracting userid from google token response failed")
      .liftState
    _ <- pushComment("Code exchange worked. User " + userId + " logged in.")
      .liftState
    _ <- auth.setUserId(userId).liftState
    _ = flow.createAndStoreCredential(tokenResponse, userId)
  } yield 1

  def startOAuth2Dance = for {
    _ <- pushComment("starting OAuth2 dance").liftState
  } yield 1

  def authServletAction: CombinedStateAndFailure[Int] = for {
    _ <- ifAll(
      predicates = List(parameterExists("error")),
      trueEffects = req => List(Comment("error parameter exists"), SetResponseContentType("text/plain"), WriteText("\"Something went wrong during auth. Please check your log for details\"")),
      trueResult = FailedCondition("The error parameter is set").left,
      falseResult = ().right).liftState

    code <- getOptionalParameter("code").liftState

    dancefloor <- code.fold(startOAuth2Dance)(c => finishOAuth2Dance(c))

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

  } yield 1
}

class AuthServlet extends ServletInjectionShim[(String, String)]()(ProjectConfiguration.configuration) {
  val servletImplementation = (new AttachmentProxyServletSupport).attachmentProxyAction
}

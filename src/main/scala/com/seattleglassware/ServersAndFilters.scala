package com.seattleglassware

import scala.collection.JavaConverters._
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
import com.google.glassware.NewUserBootstrapper
import com.google.api.services.mirror.model.Contact
import com.google.glassware.MainServlet
import com.google.api.services.mirror.model.TimelineItem
import com.google.api.services.mirror.model.NotificationConfig

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

  def getAccessToken = {
    val computation = for {
      redirectlocation <- setRedirectLocation("/oauth2callback", "failed to get auth token")
      userId <- getUserId.liftState
      credential <- getCredential(userId).liftState
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

  def finishOAuth2Dance(code: String) = for {
    _ <- pushComment("finishing OAuth2 dance").liftState

    redirectUri <- getGenericUrl.liftState
    flow <- newAuthorizationCodeFlow.liftState
    url <- getGenericUrl.map(_.newRawPath("/oauth2callback").toString)

    tokenResponse <- flow.newTokenRequest(code)
      .setRedirectUri(url)
      .execute
      .catchExceptionsT("newTokenRequest failed")

    userId <- tokenResponse.asInstanceOf[GoogleTokenResponse]
      .parseIdToken
      .getPayload
      .getUserId
      .catchExceptionsT("extracting userid from google token response failed")

    _ <- pushCommentT("Code exchange worked. User " + userId + " logged in.")

    _ <- setUserId(userId).liftState

    _ <- flow.createAndStoreCredential(tokenResponse, userId)
      .catchExceptionsT("createAndStoreCredential failed")

    _ <- bootstrapNewUser(userId)

    destination <- setRedirectLocation("/", "finished oauth2 dance")
  } yield "finishOAuth2Dance"

  def bootstrapNewUser(userId: String) = for {
    _ <- pushComment("bootstrapping new user").liftState

    credential <- getCredential(userId).liftState

    catUrl <- getGenericUrl
    imageUrls = List(catUrl.newRawPath("/static/images/chipotle-tube-640x360.jpg"))

    starterProjectContact <- (new Contact)
      .setId(MainServlet.CONTACT_NAME)
      .setDisplayName(MainServlet.CONTACT_NAME)
      .setImageUrls(imageUrls.map(_.toString).asJava)
      .catchExceptionsT("failed to create new contact")

    insertedContact <- insertContact(credential, starterProjectContact).liftState
    subscription <- insertSubscription(
      credential,
      catUrl.newRawPath("/notify").toString,
      userId,
      "timeline")
      .catchExceptionsT("Failed to create timeline subscription. Might be running on localhost")

    timelineItem <- (new TimelineItem)
      .setText("Welcome to the Glass Java Quick Start")
      .setNotification(new NotificationConfig().setLevel("DEFAULT"))
      .liftState
  } yield subscription

  def startOAuth2Dance = for {
    _ <- pushComment("starting OAuth2 dance").liftState
  } yield "startOAuth2Dance"

  def authServletAction = for {
    _ <- ifAll(
      predicates = List(parameterExists("error")),
      trueEffects = req => List(Comment("error parameter exists"), SetResponseContentType("text/plain"), WriteText("\"Something went wrong during auth. Please check your log for details\"")),
      trueResult = FailedCondition("The error parameter is set").left,
      falseResult = ().right)

    code <- getOptionalParameter("code")

    result <- code.fold(startOAuth2Dance)(c => finishOAuth2Dance(c))
  } yield result
}

class AuthServlet extends ServletInjectionShim[String]()(ProjectConfiguration.configuration) {
  val servletImplementation = (new AuthServletSupport).authServletAction
}


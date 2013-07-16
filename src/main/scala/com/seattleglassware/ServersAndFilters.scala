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
import com.google.api.services.mirror.model.TimelineItem
import com.google.api.services.mirror.model.NotificationConfig
import scala.PartialFunction._
import com.google.glassware.MainServlet
import com.seattleglassware.GlasswareTypes.stateTypes._

import HttpRequestWrapper._
import com.seattleglassware.Misc._
import com.seattleglassware.GlasswareTypes._

class AuthFilterSupport(implicit val bindingModule: BindingModule) extends StatefulParameterOperations with Injectable {
  import com.seattleglassware.GlasswareTypes.stateTypes._

  import bindingModule._

  def appspotHttpsCheck = for {
    hostnameMatches <- hostnameMatches(_.contains("appspot.com"))
    isInsecure <- urlComponentMatches(_.getScheme)(_ == "http", "failed to get http scheme")
    redirectRequired <- if (hostnameMatches && isInsecure) redirectToHttps else noOp
  } yield ()

  def middleOfAuthFlowCheck = yieldToNextFilterIfFirstElementOfPathMatches("oauth2callback", "in middle of oauth2 callback")

  def yieldWithComment(s: String) = for {
    _ <- pushComment("yielding to next filter")
    _ <- pushComment(s)
    _ <- YieldToNextFilter.liftState
  } yield ()

  def yieldToNextFilterIfFirstElementOfPathMatches(s: String, explanation: String) = for {
    pathStartsWithNotify <- urlPathMatches(cond(_) { case h :: t if h == s => true }, "failed to get path")
    _ <- if (pathStartsWithNotify) yieldWithComment(explanation) else noOp
  } yield ()

  def isRobotCheck = yieldToNextFilterIfFirstElementOfPathMatches("notify", "Skipping auth check for notify servlet")

  def getAccessToken: CombinedStateAndFailure[String] = {
    val tokenFetcher = for {
      userId <- getUserId
      credential <- getCredential(userId)
      accessToken <- safelyCall(credential.getAccessToken)(
        returnedValid = _.right[EarlyReturn],
        returnedNull = FailedCondition("no access token").left,
        threwException = t => WrappedFailure(t).left).liftState
    } yield accessToken

    // The first computation can fail with a left value.  If that
    // happens, redirect to /oauth2callback

    for {
      failureUrl <- getGenericUrlWithNewPath("/oauth2callback")
      result <- tokenFetcher.leftMap(error => ExecuteRedirect(failureUrl, "failed to get access token"))
    } yield result
  }

  def authenticationCheck = for {
    _ <- pushComment("start AuthFilter authentication check")
    _ <- appspotHttpsCheck
    _ <- middleOfAuthFlowCheck
    _ <- isRobotCheck
    _ <- getAccessToken
    _ <- pushComment("finished authentication check")
    _ <- YieldToNextFilter.liftState
  } yield ""
}

class AuthFilter extends FilterInjectionShim()(ProjectConfiguration.configuration) with NonInitializedFilter {
  val filterImplementation = (new AuthFilterSupport).authenticationCheck
}

// ----- 

class AuthServletSupport(implicit val bindingModule: BindingModule) extends StatefulParameterOperations with Injectable {
  val mirrorOps = inject[MirrorOps]
  import mirrorOps._

  def startOAuth2Dance = for {
    _ <- pushComment("starting OAuth2 dance")
    flow <- newAuthorizationCodeFlow.liftState
    redirectUrl <- getGenericUrlWithNewPath("/oauth2callback")
    authorizationCodeRequestUrl <- flow.newAuthorizationUrl()
      .setRedirectUri(redirectUrl.toString)
      .set("approval_prompt", "force")
      .catchExceptionsT("failed to create new authorization url")

    _ <- ExecuteRedirect(authorizationCodeRequestUrl, "continuing oauth2 dance by redirecting to the remote auth url").liftState
  } yield ()

  def finishOAuth2Dance(code: String) = for {
    _ <- pushComment("finishing OAuth2 dance")

    flow <- newAuthorizationCodeFlow.liftState
    oauth2callbackUrl <- getGenericUrlWithNewPath("/oauth2callback")

    tokenResponse <- flow.newTokenRequest(code)
      .setRedirectUri(oauth2callbackUrl.toString)
      .execute
      .catchExceptionsT("newTokenRequest failed")

    userId <- tokenResponse.asInstanceOf[GoogleTokenResponse]
      .parseIdToken
      .getPayload
      .getUserId
      .catchExceptionsT("extracting userid from google token response failed")

    _ <- pushComment("Code exchange worked. User " + userId + " logged in.")

    _ <- setUserId(userId)

    _ <- flow.createAndStoreCredential(tokenResponse, userId)
      .catchExceptionsT("createAndStoreCredential failed")

    state <- get[GlasswareState].liftState
    _ = println(s"current state is $state")

    _ <- bootstrapNewUser(userId)

    destination <- getGenericUrlWithNewPath("/")

    _ <- ExecuteRedirect(destination, "finished oauth2 dance").liftState
  } yield ()

  def bootstrapNewUser(userId: String) = for {
    _ <- pushComment("bootstrapping new user")

    credential <- getCredential(userId)

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

  def authServletAction = for {
    _ <- ifAll(
      predicates = List(parameterExists("error")),
      trueEffects = req => List(
        Comment("error parameter exists"),
        SetResponseContentType("text/plain"),
        WriteText("\"Something went wrong during auth. Please check your log for details\"")),
      trueResult = FailedCondition("The error parameter is set").left,
      falseResult = ().right)

    code <- getOptionalParameter("code")

    result <- code.fold(startOAuth2Dance)(finishOAuth2Dance(_))
  } yield result
}

class AuthServlet extends ServletInjectionShim[Unit]()(ProjectConfiguration.configuration) {
  override val implementationOfGet = (new AuthServletSupport).authServletAction
}

class MainServletSupport(implicit val bindingModule: BindingModule) extends StatefulParameterOperations with Injectable {
  val mirrorOps = inject[MirrorOps]
  import mirrorOps._

  def mainservletAction = for {
    operation <- getParameter("operation")
    _ <- pushComment(s"executing operation $operation")
    _ <- operation match {
      case "insertSubscription" => mainInsertSubscription
      case _                    => noOp
    }
  } yield ()

  def mainInsertSubscription = for {
    userId <- getUserId
    credential <- getCredential(userId)
    collection <- getParameter("collection")
    callbackUrl <- getGenericUrlWithNewPath("/notify")
    _ <- insertSubscription(credential, callbackUrl.toString, userId, collection)
      .catchExceptionsT("Could not subscribe $callbackUrl $collection")
  } yield collection

  def mainDeleteSubscription = for {
    userId <- getUserId
    credential <- getCredential(userId)
    subscriptionId <- getParameter("subscriptionId")
    _ <- deleteSubscription(credential, subscriptionId)
  } yield subscriptionId
}

class MainServlet extends ServletInjectionShim[Unit]()(ProjectConfiguration.configuration) {
  override val implementationOfPost = (new MainServletSupport).mainservletAction
}
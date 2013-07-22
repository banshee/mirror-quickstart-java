package com.seattleglassware

import com.google.glassware.AuthUtil
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
import scalaz._
import scalaz.Scalaz._
import scalaz.State
import scalaz.StateT
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
import com.google.api.client.http.GenericUrl
import java.net.URL
import com.google.glassware.MirrorClient
import com.google.api.services.mirror.model.MenuItem
import com.google.api.services.mirror.model.MenuValue
import com.google.api.client.googleapis.batch.json.JsonBatchCallback
import com.google.api.client.googleapis.json.GoogleJsonError
import com.google.api.client.http.HttpHeaders
import com.google.api.client.googleapis.batch.BatchRequest

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
      credential <- getCredential
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
  } yield ()
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

    _ <- redirectToNewPath("/", "finished oauth2 dance")
  } yield ()

  def bootstrapNewUser(userId: String) = for {
    _ <- pushComment("bootstrapping new user")

    credential <- getCredential

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

class AuthServlet extends ServletInjectionShim()(ProjectConfiguration.configuration) {
  override val implementationOfGet = (new AuthServletSupport).authServletAction
}

class MainServletSupport(implicit val bindingModule: BindingModule) extends StatefulParameterOperations with Injectable {
  val m = inject[MirrorOps]

  class BatchCallback extends JsonBatchCallback[TimelineItem] {
    override def onSuccess(t: TimelineItem, responseHeaders: HttpHeaders) = {

    }

    override def onFailure(error: GoogleJsonError, responseHeaders: HttpHeaders) = {

    }
  }

  implicit class OrElseMessage[T](x: CombinedStateAndFailure[T]) {
    def orElseMessage(s: String) = {
      def prependStateMsg(e: EarlyReturn) = for {
        _ <- addGlasswareEffect(LogAndIgnore(e))
        _ <- addGlasswareEffect(AddMessage(s))
      } yield ()
      val ignoringRight = x.map(rightvalue => ())
      transformLeft(ignoringRight)(error => prependStateMsg(error))
    }
  }

  def mainservletAction = for {
    operation <- getParameter("operation")
    _ <- pushComment(s"executing operation $operation")
    _ <- operation match {
    case "insertSubscription" =>
    insertSubscription orElseMessage "Failed to subscribe. Check your log for details"
      case "deleteSubscription" =>
        deleteSubscription orElseMessage "Failed to delete subscription"
      case "insertItem" =>
        insertItem orElseMessage "Failed to insert item"
      case "insertContact" =>
        insertContact orElseMessage "Failed to insert contact"
      case "deleteContact" =>
        deleteContact orElseMessage "Failed to delete contact"
      case "insertItemWithAction" =>
        insertItemWithAction orElseMessage "Failed to insert item"
      case "insertItemAllUsers" =>
        insertItemAllUsers orElseMessage "Failed to insert items for all users"
      case unknownCommand => modifyGlasswareEffects(
        xs => Comment(s"Unknown commmand $unknownCommand") :: AddMessage("I don't know how to do that") :: xs)
    }
    _ <- redirectToNewPath("/", s"finished operation $operation")
  } yield ()

  def insertItemWithAction = for {
    timelineItem <- (
      (new TimelineItem)
      .setText("Tell me what you had for lunch :)"))
      .catchExceptionsT("failed to create TimelineItem object")
    drillImageUrl <- getGenericUrlWithNewPath("/static/images/drill.png")
    menuValues = List(
      (new MenuValue)
        .setIconUrl(drillImageUrl.toString)
        .setDisplayName("Drill In"))
    menuItemList = List(
      (new MenuItem).setAction("REPLY"),
      (new MenuItem).setAction("READ_ALOUD"),
      (new MenuItem).setValues(menuValues.asJava).setId("drill").setAction("CUSTOM"))
    timelineItem <- timelineItem
      .setMenuItems(menuItemList.asJava)
      .setNotification((new NotificationConfig).setLevel("DEFAULT"))
      .catchExceptionsT("could not build timeline item")
    userId <- getUserId
    credential <- getCredential
    insertedTimelineItem <- m.insertTimelineItemWithoutContent(credential, timelineItem)
    _ <- addGlasswareEffect(AddMessage("A timeline item with actions has been inserted."))
  } yield insertedTimelineItem

  def insertItem = for {
    timelineItem <- (new TimelineItem)
      .catchExceptionsT("failed to create TimelineItem object")

    message <- getOptionalParameter("message")
    _ <- timelineItem.setText(message | null)
      .catchExceptionsT("could not set text")

    _ <- timelineItem
      .setNotification((new NotificationConfig).setLevel("DEFAULT"))
      .catchExceptionsT("unable to set notification level")

    imageUrlParameter <- getOptionalParameter("imageUrl")

    // If the url is provided, make sure it's really a url
    optionalUrlObject <- (imageUrlParameter map { x => new URL(x) })
      .catchExceptionsT(s"could not create url from $imageUrlParameter")

    contentTypeParameter <- getOptionalParameter("contentType")
    userId <- getUserId
    credential <- getCredential
    _ <- (optionalUrlObject, contentTypeParameter) match {
      case (Some(i), Some(c)) =>
        m.insertTimelineItemUsingUrl(credential,
          timelineItem,
          c,
          i)
      case (None, None) =>
        m.insertTimelineItemWithoutContent(credential, timelineItem)
      case x =>
        FailedCondition(s"need both content type and url, got: $x").failure
    }
    _ <- addGlasswareEffect(AddMessage("A timeline item has been inserted."))
  } yield userId

  def insertSubscription = for {
    userId <- getUserId
    credential <- getCredential
    collection <- getParameter("collection")
    callbackUrl <- getGenericUrlWithNewPath("/notify")
    subscription <- m.insertSubscription(credential, callbackUrl.toString, userId, collection)
    _ <- addGlasswareEffect(AddMessage("Application is now subscribed to updates."))
  } yield subscription

  def insertContact = for {
    _ <- pushComment("Inserting contact item")
    iconUrl <- getParameter("iconUrl")
    name <- getParameter("name")
    userId <- getUserId
    credential <- getCredential
    contact <- (new Contact)
      .setId(name)
      .setDisplayName(name)
      .setImageUrls(List(iconUrl).asJava)
      .catchExceptionsT("could not build contact")
    _ <- m.insertContact(credential, contact)
    _ <- message(s"Inserted contact: $name")
  } yield 1

  def deleteSubscription = for {
    credential <- getCredential
    subscriptionId <- getParameter("subscriptionId")
    _ <- m.deleteSubscription(credential, subscriptionId)
  } yield subscriptionId

  def deleteContact = for {
    credential <- getCredential
    id <- getParameter("id")
    _ <- m.deleteContact(credential, id)
    _ <- message("Contact has been deleted.")
  } yield ()

  def ifCondition(condition: => Boolean, result: CombinedStateAndFailure[_]) =
    if (condition) result else noOp

  def ifOption[T, U](opt: => Option[T], result: T => CombinedStateAndFailure[U]): CombinedStateAndFailure[_] =
    if (opt.isDefined) result(opt.get) else noOp

  def insertItemAllUsers = for {
    url <- getGenericUrl

    _ <- ifCondition(url.getHost.contains("glass-java-starter-demo.appspot.com"),
      FinishedProcessing("This function is disabled on the demo instance").exitProcessing)

    users = AuthUtil.getAllUserIds.asScala.toList

    _ <- ifCondition(users.size > 10,
      FailedCondition("too many users").failure)

    allUsersItem = (new TimelineItem).setText("Hello Everyone!")

    _ <- queueTimelineInsertions(users, allUsersItem)
  } yield ()

  def queueTimelineInsertions(users: List[String], allUsersItem: TimelineItem) = for {
    batch <- m.getBatch
    callback = new BatchCallback
    userIds <- users.foldLeft(zero)((acc, userid) => acc +++ queueTimelineInsertion(userid, allUsersItem, batch, callback))
    _ <- batch.execute
      .catchExceptionsT("failed to execute batch for inserting item for all users")
    _ <- message(s"Successfully sent cards to ${callback.success} users + ${callback.failure} failed.")
  } yield userIds

  def queueTimelineInsertion(userId: String, allUsersItem: TimelineItem, batch: BatchRequest, callback: BatchCallback) = for {
    credential <- m.getCredentialForSpecifiedUser(userId)
    mirror <- m.getMirror(credential)
    result <- mirror
      .timeline
      .insert(allUsersItem)
      .queue(batch, callback)
      .catchExceptionsT("unable to create batch timeline item")
  } yield List(userId) // yielding a List[String] so I can use +++ to combine the EitherT items in queueTimelineInsertions

  val zero = EitherT(Applicative[StateWithFixedStateType].point(List.empty[String].right[EarlyReturn]))
}

class MainServlet extends ServletInjectionShim()(ProjectConfiguration.configuration) {
  override val implementationOfPost = (new MainServletSupport).mainservletAction
}

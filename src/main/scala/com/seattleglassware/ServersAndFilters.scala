package com.seattleglassware

import java.io.InputStreamReader
import java.io.BufferedReader
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
import java.util.concurrent.atomic.AtomicInteger
import java.io.IOException
import com.google.api.client.json.jackson.JacksonFactory
import com.google.api.services.mirror.model.Notification
import com.google.api.services.mirror.model.UserAction
import com.google.api.client.auth.oauth2.Credential

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
    flow <- newAuthorizationCodeFlow
    redirectUrl <- getGenericUrlWithNewPath("/oauth2callback")
    authorizationCodeRequestUrl <- flow.newAuthorizationUrl
      .setRedirectUri(redirectUrl.toString)
      .set("approval_prompt", "force")
      .mapExceptionOrNullToLeft("failed to create new authorization url")

    _ <- ExecuteRedirect(authorizationCodeRequestUrl, "continuing oauth2 dance by redirecting to the remote auth url").liftState
  } yield ()

  def finishOAuth2Dance(code: String) = for {
    _ <- pushComment("finishing OAuth2 dance")

    flow <- newAuthorizationCodeFlow
    oauth2callbackUrl <- getGenericUrlWithNewPath("/oauth2callback")

    tokenResponse <- flow.newTokenRequest(code)
      .setRedirectUri(oauth2callbackUrl.toString)
      .execute
      .mapExceptionToLeft("newTokenRequest failed")

    userId <- tokenResponse.asInstanceOf[GoogleTokenResponse]
      .parseIdToken
      .getPayload
      .getUserId
      .mapExceptionToLeft("extracting userid from google token response failed")

    _ <- pushComment("Code exchange worked. User " + userId + " logged in.")

    _ <- setUserId(userId)

    responseToken <- flow.createAndStoreCredential(tokenResponse, userId)
      .mapExceptionOrNullToLeft("createAndStoreCredential failed")

    _ <- bootstrapNewUser(userId)

    _ <- redirectToNewPath("/", "finished oauth2 dance")
  } yield ()

  def capableOfSubscriptions = false

  def bootstrapNewUser(userId: String) = for {
    _ <- pushComment("bootstrapping new user")

    credential <- getCredential

    catUrl <- getGenericUrl
    imageUrls = List(catUrl.newRawPath("/static/images/chipotle-tube-640x360.jpg"))

    starterProjectContact <- (new Contact)
      .setId(MainServlet.CONTACT_NAME)
      .setDisplayName(MainServlet.CONTACT_NAME)
      .setImageUrls(imageUrls.map(_.toString).asJava)
      .mapExceptionOrNullToLeft("failed to create new contact")

    insertedContact <- insertContact(credential, starterProjectContact).liftState
    subscription <- if (capableOfSubscriptions)
      insertSubscription(
        credential,
        catUrl.newRawPath("/notify").toString,
        userId,
        "timeline")
    else noOp

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
      case s =>
        unknownCommand(s)
    }
    _ <- redirectToNewPath("/", s"finished operation $operation")
  } yield ()

  def insertItemWithAction = for {
    timelineItem <- (
      (new TimelineItem)
      .setText("Tell me what you had for lunch :)"))
      .mapExceptionOrNullToLeft("failed to create TimelineItem object")
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
      .mapExceptionOrNullToLeft("could not build timeline item")
    userId <- getUserId
    credential <- getCredential
    insertedTimelineItem <- m.insertTimelineItemWithoutContent(credential, timelineItem)
    _ <- addGlasswareEffect(AddMessage("A timeline item with actions has been inserted."))
  } yield insertedTimelineItem

  def insertItem = for {
    timelineItem <- (new TimelineItem)
      .mapExceptionOrNullToLeft("failed to create TimelineItem object")

    message <- getOptionalParameter("message")
    _ <- timelineItem
      .setText(message | null)
      .setNotification((new NotificationConfig).setLevel("DEFAULT"))
      .mapExceptionOrNullToLeft("unable to set notification level")

    imageUrlParameter <- getOptionalParameter("imageUrl")

    // If the url is provided, make sure it's really a url
    optionalUrlObject <- (imageUrlParameter map { x => new URL(x) })
      .mapExceptionOrNullToLeft(s"could not create url from $imageUrlParameter")

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
      .mapExceptionOrNullToLeft("could not build contact")
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

  def ifCondition[T](condition: => Boolean, result: CombinedStateAndFailure[T]) =
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

  class BatchCallback extends JsonBatchCallback[TimelineItem] {
    val successCount = new AtomicInteger(0)
    val failureCount = new AtomicInteger(0)

    override def onSuccess(t: TimelineItem, responseHeaders: HttpHeaders) =
      successCount.incrementAndGet

    override def onFailure(error: GoogleJsonError, responseHeaders: HttpHeaders) =
      failureCount.incrementAndGet
  }

  def queueTimelineInsertions(users: List[String], allUsersItem: TimelineItem) = for {
    batch <- m.getBatch
    callback = new BatchCallback
    userIds <- users.foldLeft(zero)((acc, userid) => acc +++ queueTimelineInsertion(userid, allUsersItem, batch, callback))
    _ <- batch.execute
      .mapExceptionToLeft("failed to execute batch for inserting item for all users")
    _ <- message(s"Successfully sent cards to ${callback.successCount.get} users + ${callback.failureCount.get} failed.")
  } yield userIds

  def queueTimelineInsertion(userId: String, allUsersItem: TimelineItem, batch: BatchRequest, callback: BatchCallback) = for {
    credential <- m.getCredentialForSpecifiedUser(userId)
    mirror <- m.getMirror(credential)
    result <- mirror
      .timeline
      .insert(allUsersItem)
      .queue(batch, callback)
      .mapExceptionToLeft("unable to create batch timeline item")
  } yield List(userId) // yielding a List[String] so I can use +++ to combine the EitherT items in queueTimelineInsertions

  val zero = EitherT(Applicative[StateWithFixedStateType].point(List.empty[String].right[EarlyReturn]))
}

class MainServlet extends ServletInjectionShim()(ProjectConfiguration.configuration) {
  override val implementationOfPost = (new MainServletSupport).mainservletAction
}

class NotifyServletImpl(implicit val bindingModule: BindingModule) extends StatefulParameterOperations with Injectable {
  import bindingModule._

  val jsonFactory = inject[JacksonFactory]
  val m = inject[MirrorOps]

  def handlePost = for {
    _ <- pushEffect(SetResponseContentType("text/html"))
    _ <- pushEffect(WriteText("OK"))
    notificationReader <- getInputBufferedReader
    // Count the lines as a very basic way to prevent Denial of Service attacks
    notificationString <- readUpToNLines(notificationReader, 1000)
      .mapExceptionToLeft("Attempted to parse notification payload that was unexpectedly long.")

    _ <- pushComment("got raw notification " + notificationString)

    notification <- jsonFactory.fromString(notificationString.toString, classOf[Notification])
      .mapExceptionOrNullToLeft(s"could not build notification from $notificationString")

    command <- notification.getCollection
      .mapExceptionOrNullToLeft("could not get notification collection name")

    _ <- pushComment(s"got command $command")

    _ <- command match {
      case "locations" =>
        locations(notification) orElseMessage "could not update location"
      case "timeline" =>
        timeline(notification) orElseMessage "could not update timeline"
      case s =>
        unknownCommand(s)
    }
  } yield ()

  def timeline(notification: Notification) = for {
    credential <- getCredentialForNotification(notification)
    mirror <- m.getMirror(credential)
    timelineItem <- mirror.timeline
      .get(notification.getItemId)
      .execute
      .mapExceptionToLeft("unable to get notification from timeline")
    _ <- pushComment(s"Notification impacted timeline item with ID: ${timelineItem.getId}")
    // If it was a share, and contains a photo, bounce it back to the user.
    containsShareWithPhoto = (notification.getUserActions().contains(new UserAction().setType("SHARE"))
      && timelineItem.getAttachments() != null && timelineItem.getAttachments().size() > 0)
    _ <- if (containsShareWithPhoto)
      sendPhotoBackToUser(timelineItem, credential)
    else
      noOp
  } yield timelineItem

  def sendPhotoBackToUser(timelineItem: TimelineItem, credential: Credential) = for {
    mirror <- m.getMirror(credential)
    _ <- pushComment("It was a share of a photo. Sending the photo back to the user.")
    attachmentId = timelineItem.getAttachments().get(0).getId()
    _ <- pushComment("Found attachment with ID " + attachmentId)
    stream <- m.getAttachmentInputStream(credential, timelineItem.getId, attachmentId)
    echoPhotoItem = (new TimelineItem).setText("Echoing your shared photo")
    insertedItem <- m.insertTimelineItemUsingStream(credential, echoPhotoItem, "image/jpeg", stream)
  } yield insertedItem

  def locations(notification: Notification) = for {
    credential <- getCredentialForNotification(notification)
    mirror <- m.getMirror(credential)

    location <- mirror.locations
      .get(notification.getItemId)
      .execute
      .mapExceptionToLeft("could not get location")

    _ <- pushComment(s"New location is ${location.getLatitude}, ${location.getLongitude}")

    timelineItem <- (new TimelineItem())
      .setText(s"You are now at ${location.getLatitude}, ${location.getLongitude}")
      .setNotification(new NotificationConfig().setLevel("DEFAULT")).setLocation(location)
      .setMenuItems(List((new MenuItem).setAction("NAVIGATE")).asJava)
      .mapExceptionOrNullToLeft("could not build timeline item")

    insertedItem <- m.insertTimelineItemWithoutContent(credential, timelineItem)
  } yield insertedItem

  def readUpToNLines(reader: scala.io.Source, n: Int) =
    reader.getLines
      .takeWhile(doAfterNTimes(n, throw new IOException("Attempted to parse notification payload that was unexpectedly long.")))
      .foldLeft(new StringBuffer)((acc, s) => acc.append(s))

  def notNull[T](t: T) = t != null

  def doAfterNTimes[T](n: Int, ex: => Unit) = {
    var i = 0
    (s: T) => {
      if (i >= n) ex
      i += 1
      true
    }
  }
}

class NotifyServlet extends ServletInjectionShim()(ProjectConfiguration.configuration) {
  override val implementationOfPost = (new NotifyServletImpl).handlePost
}

class SignOutServletImpl(implicit val bindingModule: BindingModule) extends StatefulParameterOperations with Injectable {
  import bindingModule._

  def handlePost = for {
    _ <- addGlasswareEffect(SignOut)
  } yield ()
}

class SignOutServlet extends ServletInjectionShim()(ProjectConfiguration.configuration) {
  override val implementationOfPost = (new SignOutServletImpl).handlePost
}


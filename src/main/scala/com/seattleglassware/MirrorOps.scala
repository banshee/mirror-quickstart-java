package com.seattleglassware

import scala.collection.JavaConverters._
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
import scalaz.Bind
import scalaz.Applicative
import scalaz.MonadTrans
import scalaz.Scalaz._
import scalaz.State
import scalaz.\/
import scalaz.-\/
import scalaz.\/-
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
import com.google.api.client.auth.oauth2.CredentialStoreRefreshListener
import com.google.api.services.mirror.model.Contact
import com.google.api.services.mirror.model.Subscription
import HttpRequestWrapper._
import com.seattleglassware.Misc.GenericUrlWithNewScheme
import com.seattleglassware.GlasswareTypes.stateTypes._

class MirrorOps(implicit val bindingModule: BindingModule) extends Injectable with StatefulParameterOperations {
  import com.seattleglassware.Misc._
  import HttpRequestWrapper._
  import stateTypes._

  private lazy val urlFetchTransport = inject[UrlFetchTransport]
  private lazy val jacksonFactory = inject[JacksonFactory]
  private lazy val applicationName = inject[String](ApplicationName)

  def getAttachmentInputStream(credential: Credential, timelineItemId: String, attachmentId: String) = for {
    mirror <- getMirror(credential)
    attachmentsMetadata <- getAttachmentMetadata(mirror, timelineItemId, attachmentId)
    url <- attachmentsMetadata.getContentUrl
      .catchExceptionsT("could not get content url for attachment")
    genericUrl <- new GenericUrl(url)
      .catchExceptionsT(s"could not build genericUrl from [$url]")
    request = mirror.getRequestFactory.buildGetRequest(genericUrl)
    resp <- request.execute
      .catchExceptionsT("error fetching a mirror request")
    content <- resp.getContent
      .catchExceptionsT("error getting the content of an attachment")
  } yield content

  def getAttachmentMetadata(mirror: Mirror, timelineItemId: String, attachmentId: String) = {
    for {
      attachments <- mirror.timeline.attachments
        .catchExceptionsT("failed to get timeline attachments")
      attachmentsRequest <- attachments.get(timelineItemId, attachmentId)
        .catchExceptionsT("error creating attachments request")
      attachmentsMetadata <- attachmentsRequest.execute
        .catchExceptionsT("error executing attachments fetch")
    } yield attachmentsMetadata
  }

  def getMirror(credential: Credential) = for {
    result <- new Mirror.Builder(urlFetchTransport, jacksonFactory, credential)
      .setApplicationName(applicationName)
      .build()
      .catchExceptionsT("failed to build a mirror object")
  } yield result

  def getAttachmentContentType(mirror: Mirror, timelineItemId: String, attachmentId: String) = for {
    metadata <- getAttachmentMetadata(mirror, timelineItemId, attachmentId)
    contentType <- metadata.getContentType.catchExceptionsT("no content type")
  } yield contentType

  def insertContact(credential: Credential, contact: Contact) = {
    getMirror(credential).map {
      _.contacts.insert(contact).execute
    }
  }

  def insertSubscription(credential: Credential, callbackUrl: String, userId: String, collection: String) = for {
    mirror <- getMirror(credential)
    subscription <- (new Subscription)
      .setCollection(collection)
      .setCallbackUrl(callbackUrl)
      .setUserToken(userId)
      .catchExceptionsT("failed to build subscription")
    insertedSubscription <- mirror.subscriptions
      .insert(subscription)
      .execute
      .catchExceptionsT("failed to insert subscription.")
  } yield subscription

  def deleteSubscription(credential: Credential, subscriptionId: String) = for {
    mirror <- getMirror(credential)
    subs <- mirror.subscriptions
      .catchExceptionsT("could not get subscriptions")
    deleteThunk <- subs.delete(subscriptionId)
      .catchExceptionsT("could not delete subscription")
    _ <- deleteThunk.execute
      .catchExceptionsT("executing delete command failed")
  } yield ()
}

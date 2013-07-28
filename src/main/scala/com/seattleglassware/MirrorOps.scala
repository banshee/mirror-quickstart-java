package com.seattleglassware

import GlasswareTypes._
import HttpRequestWrapper._
import JavaInterop.asInstanceOfNotNull
import JavaInterop.safelyCall
import com.escalatesoft.subcut.inject.BindingModule
import com.escalatesoft.subcut.inject.Injectable
import com.escalatesoft.subcut.inject.bindingIdToString
import com.google.api.client.auth.oauth2.AuthorizationCodeFlow
import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.auth.oauth2.Credential.AccessMethod
import com.google.api.client.auth.oauth2.CredentialStore
import com.google.api.client.auth.oauth2.CredentialStoreRefreshListener
import com.google.api.client.auth.oauth2.TokenResponse
import com.google.api.client.extensions.appengine.http.UrlFetchTransport
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
import com.google.api.client.http.ByteArrayContent
import com.google.api.client.http.GenericUrl
import com.google.api.client.http.HttpExecuteInterceptor
import com.google.api.client.http.HttpRequestInitializer
import com.google.api.client.json.jackson.JacksonFactory
import com.google.api.client.util.Clock
import com.google.api.services.mirror.Mirror
import com.google.api.services.mirror.model.Contact
import com.google.api.services.mirror.model.NotificationConfig
import com.google.api.services.mirror.model.Subscription
import com.google.api.services.mirror.model.TimelineItem
import com.google.common.io.ByteStreams
import com.google.glassware.MirrorClient
import com.seattleglassware.BindingIdentifiers._
import com.seattleglassware.EitherTWithState._
import com.seattleglassware.GlasswareTypes._
import com.seattleglassware.GlasswareTypes.stateTypes._
import com.seattleglassware.Misc.GenericUrlWithNewScheme
import com.seattleglassware.Misc._
import java.io.FileInputStream
import java.io.InputStream
import java.net.URL
import java.util.Properties
import javax.servlet.Filter
import javax.servlet.FilterChain
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import scala.PartialFunction._
import scala.collection.JavaConverters._
import scalaz.-\/
import scalaz.Applicative
import scalaz.Bind
import scalaz.EitherT
import scalaz.Lens
import scalaz.MonadTrans
import scalaz.Monoid
import scalaz.Scalaz._
import scalaz.State
import scalaz.\/
import scalaz.\/-

class MirrorOps(implicit val bindingModule: BindingModule) extends Injectable with StatefulParameterOperations {
  import com.seattleglassware.Misc._
  import HttpRequestWrapper._

  private lazy val urlFetchTransport = inject[UrlFetchTransport]
  private lazy val jacksonFactory = inject[JacksonFactory]
  private lazy val applicationName = inject[String](ApplicationName)

  def getAttachmentInputStream(credential: Credential, timelineItemId: String, attachmentId: String) = for {
    mirror <- getMirror(credential)
    attachmentsMetadata <- getAttachmentMetadata(mirror, timelineItemId, attachmentId)
    url <- Option(attachmentsMetadata.getContentUrl)
      .mapExceptionToLeft("could not get content url for attachment")
    genericUrl <- new GenericUrl(url | null)
      .mapExceptionToLeft(s"could not build genericUrl from [$url]")
    resp <- mirror
      .getRequestFactory
      .buildGetRequest(genericUrl)
      .execute
      .mapExceptionToLeft("error fetching a mirror request")
    content <- resp.getContent
      .mapExceptionToLeft("error getting the content of an attachment")
    _ <- pushEffect(CleanupCloseable(content))
  } yield content

  def getAttachmentMetadata(mirror: Mirror, timelineItemId: String, attachmentId: String) = {
    for {
      attachments <- mirror
        .timeline
        .attachments
        .mapExceptionOrNullToLeft("Failed to get attachments")
      attachmentsRequest <- attachments.get(timelineItemId, attachmentId)
        .mapExceptionOrNullToLeft("error creating attachments request")
      attachmentsMetadata <- attachmentsRequest.execute
        .mapExceptionToLeft("error executing attachments fetch")
    } yield attachmentsMetadata
  }

  def getMirror(credential: Credential) = for {
    result <- new Mirror.Builder(urlFetchTransport, jacksonFactory, credential)
      .setApplicationName(applicationName)
      .build()
      .mapExceptionOrNullToLeft("failed to build a mirror object")
  } yield result

  def getAttachmentContentType(mirror: Mirror, timelineItemId: String, attachmentId: String) = for {
    metadata <- getAttachmentMetadata(mirror, timelineItemId, attachmentId)
    // note that null means no content type,
    // but if you're calling this method then no content type
    // is fatal
    contentType <- metadata.getContentType
      .mapExceptionToLeft("no content type")
  } yield contentType

  def insertContact(credential: Credential, contact: Contact) = for {
    m <- getMirror(credential)
    c <- m.contacts
      .insert(contact)
      .execute
      .mapExceptionToLeft("unable to insert contact")
  } yield c

  def insertTimelineItem(credential: Credential, timelineItem: TimelineItem, contentType: String, attachmentData: Array[Byte]) = for {
    mirror <- getMirror(credential)
    contentBytes <- (new ByteArrayContent(contentType, attachmentData))
      .mapExceptionOrNullToLeft("unable to build content for attachment")
    item <- mirror
      .timeline
      .insert(timelineItem, contentBytes)
      .execute
      .mapExceptionToLeft("failed to insert timeline item with attachment as byte array")
  } yield item

  def insertTimelineItemWithoutContent(credential: Credential, timelineItem: TimelineItem) = for {
    mirror <- getMirror(credential)
    item <- mirror
      .timeline
      .insert(timelineItem)
      .execute
      .mapExceptionToLeft("failed to insert timeline item with without content")
  } yield item

  def insertTimelineItemUsingStream(credential: Credential, timelineItem: TimelineItem, contentType: String, attachmentStream: InputStream) =
    insertTimelineItem(credential, timelineItem, contentType, ByteStreams.toByteArray(attachmentStream))

  def insertTimelineItemUsingUrl(credential: Credential, timelineItem: TimelineItem, contentType: String, url: URL) = for {
    s <- url.openStream
      .mapExceptionOrNullToLeft(s"unable to open stream for url $url")
    result <- insertTimelineItemUsingStream(credential, timelineItem, contentType, s)
  } yield result

  def insertSubscription(credential: Credential, callbackUrl: String, userId: String, collection: String) = for {
    mirror <- getMirror(credential)
    subscription <- (new Subscription)
      .setCollection(collection)
      .setCallbackUrl(callbackUrl)
      .setUserToken(userId)
      .mapExceptionOrNullToLeft("failed to build subscription")
    insertedSubscription <- mirror.subscriptions
      .insert(subscription)
      .execute
      .mapExceptionToLeft("failed to insert subscription.")
  } yield subscription

  def deleteSubscription(credential: Credential, subscriptionId: String) = for {
    mirror <- getMirror(credential)
    subs <- mirror.subscriptions
      .delete(subscriptionId)
      .execute
      .mapExceptionToLeft("executing delete command failed")
  } yield ()

  def deleteContact(credential: Credential, id: String) = for {
    mirror <- getMirror(credential)
    _ <- mirror
      .contacts
      .delete(id)
      .execute
      .mapExceptionToLeft(s"could not delete contact $id")
  } yield id

  def listContacts(credential: Credential) = for {
    mirror <- getMirror(credential)
    contacts <- mirror
      .contacts
      .list
      .execute
      .mapExceptionToLeft(s"could not list contacts")
  } yield contacts

  def getContact(credential: Credential, id: String) = for {
    mirror <- getMirror(credential)
    contact <- mirror
      .contacts
      .get(id)
      .execute
      .mapExceptionToLeft(s"could not get contact $id")
  } yield contact

  def listItems(credential: Credential, count: Int) = for {
    mirror <- getMirror(credential)
    items <- mirror
      .timeline
      .list
      .setMaxResults(count)
      .execute
      .mapExceptionToLeft(s"could not get mirror items")
  } yield items

  def listSubscriptions(credential: Credential) = for {
    mirror <- getMirror(credential)
    subscriptions <- mirror
      .subscriptions
      .list
      .execute
      .mapExceptionToLeft(s"could not get mirror items")
  } yield subscriptions

  def getBatch = for {
    mirror <- getMirror(null)
    batch <- mirror.batch
      .mapExceptionToLeft("failed to create batch")
  } yield batch
}

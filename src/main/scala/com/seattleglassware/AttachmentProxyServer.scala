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

class AttachmentProxyServletSupport(implicit val bindingModule: BindingModule) extends StatefulParameterOperations with Injectable {
  import stateTypes._
  import bindingModule._

  val mirrorOps = inject[MirrorOps]
  import mirrorOps._
  
  def attachmentProxyAction: CombinedStateAndFailure[(String, String)] = for {
    attachmentId <- getParameter("attachment")
    timelineItemId <- getParameter("timelineItem")
    
    userid <- getUserId
    credential <- getCredential(userid)
    mirror <- getMirror(credential)
    contentType <- getAttachmentContentType(mirror, timelineItemId, attachmentId)
    attachmentInputStream <- getAttachmentInputStream(credential, timelineItemId, attachmentId)

    _ <- pushEffect(SetResponseContentType(contentType))
    _ <- pushEffect(CopyStreamToOutput(attachmentInputStream))

  } yield (attachmentId, timelineItemId)
}

class AttachmentProxyServlet extends ServletInjectionShim[(String, String)]()(ProjectConfiguration.configuration) {
  override val implementationOfGet = (new AttachmentProxyServletSupport).attachmentProxyAction
}

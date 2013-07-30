package com.seattleglassware

import JavaInterop.safelyCall
import com.escalatesoft.subcut.inject.BindingModule
import com.escalatesoft.subcut.inject.Injectable
import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.auth.oauth2.TokenResponse
import com.google.api.client.googleapis.auth.oauth2.GoogleTokenResponse
import com.google.api.client.googleapis.batch.BatchRequest
import com.google.api.client.googleapis.batch.json.JsonBatchCallback
import com.google.api.client.googleapis.json.GoogleJsonError
import com.google.api.client.http.GenericUrl
import com.google.api.client.http.HttpHeaders
import com.google.api.client.json.jackson.JacksonFactory
import com.google.api.services.mirror.model.Contact
import com.google.api.services.mirror.model.MenuItem
import com.google.api.services.mirror.model.MenuValue
import com.google.api.services.mirror.model.Notification
import com.google.api.services.mirror.model.NotificationConfig
import com.google.api.services.mirror.model.TimelineItem
import com.google.api.services.mirror.model.UserAction
import com.google.glassware.AuthUtil
import com.google.glassware.MirrorClient
import com.google.glassware.NewUserBootstrapper
import com.seattleglassware.GlasswareTypes._
import com.seattleglassware.GlasswareTypes.stateTypes._
import com.seattleglassware.Misc._
import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.net.URL
import scala.util.control.Exception
import scala.util.control.Exception._
import java.util.concurrent.atomic.AtomicInteger
import scala.PartialFunction._
import scala.collection.JavaConverters._
import scalaz.Scalaz._
import scalaz.State
import scalaz.StateT
import scalaz._
import javax.servlet.http.HttpServletRequest
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.http.HttpServletResponse
import javax.servlet.FilterChain
import com.google.api.client.util.ByteStreams

trait ServerPlumbing extends StatefulParameterOperations with Injectable {
  implicit val bindingModule: BindingModule

  val log = inject[java.util.logging.Logger]
  
  def doServerPlumbing[T](req: HttpServletRequest, resp: HttpServletResponse, s: CombinedStateAndFailure[T]) = {
    val (state, result) = s.run(GlasswareState(req))
    val effects = effectsThroughGlasswareState.get(state)
    executeEffects(effects, req, resp, None, result)
    executeResult(result, req, resp, None)
  }

  def doFilterPlumbing[T](req: ServletRequest, resp: ServletResponse, chain: FilterChain, s: CombinedStateAndFailure[T]): Unit = {
    (req, resp) match {
      case (req: HttpServletRequest, resp: HttpServletResponse) => doHttpFilter(req, resp, chain, s)
      case _ => ()
    }
  }
  private[this] def doHttpFilter[T](req: HttpServletRequest, resp: HttpServletResponse, chain: FilterChain, s: CombinedStateAndFailure[T]): Unit = {
    val (GlasswareState(_, effects), result) = s.run(GlasswareState(req))
    executeEffects(effects, req, resp, chain.some, result)
    executeResult(result, req, resp, chain.some)
  }

  def executeResult[T](result: EarlyReturn \/ T, req: HttpServletRequest, resp: HttpServletResponse, chain: Option[FilterChain]) = result match {
    case -\/(ExecuteRedirect(url, _)) => resp.sendRedirect(url.toString)
    case -\/(YieldToNextFilter)       => chain.get.doFilter(req, resp)
    case _                            => ()
  }

  def executeEffects[T](effects: List[Effect], req: HttpServletRequest, resp: HttpServletResponse, chain: Option[FilterChain], result: EarlyReturn \/ T) = {
    val requrl = req.getRequestURL().toString
    log.info(s"Run request =====\nearlyReturn: $result\ndoeffects:\n$requrl\n$effects\n========")
    effects.reverse foreach executeEffect(req, resp, chain, result)
  }

  def executeEffect[T](req: HttpServletRequest, resp: HttpServletResponse, chain: Option[FilterChain], result: EarlyReturn \/ T)(e: Effect) = e match {
    case SetResponseContentType(s) =>
      resp.setContentType(s)
    case CopyStreamToOutput(stream) =>
      ultimately(stream.close) {
        ByteStreams.copy(stream, resp.getOutputStream)
      }
    case WriteText(s) =>
      val writer = resp.getWriter
      writer.append("OK")
      writer.close()
    case SetSessionAttribute(name, value) =>
      Option(req.getSession) map { _.setAttribute(name, value) }
    case RemoveSessionAttribute(name) =>
      Option(req.getSession) map { _.removeAttribute(name) }
    case AddMessage(m) =>
      Option(req.getSession) map { _.setAttribute("flash", m) }
    case CleanupCloseable(c) =>
      try {
        c.close
      } catch {
        case e: java.io.IOException =>
      }
    case SignOut | LogAndIgnore(_) | Comment(_) | PlaceholderEffect => // Do nothing
  }
}

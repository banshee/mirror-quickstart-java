package com.seattleglassware

import scala.collection.JavaConverters.asScalaBufferConverter

import com.escalatesoft.subcut.inject.BindingModule
import com.google.api.client.http.GenericUrl

import GlasswareTypes._
import JavaInterop.asInstanceOfNotNull
import JavaInterop.safelyCall
import javax.servlet.Filter
import javax.servlet.FilterChain
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import scalaz.Scalaz._
import scalaz.\/

object Misc {
  type =?>[A, B] = PartialFunction[A, B]

  implicit class GenericUrlWithNewScheme(u: GenericUrl) {
    def newScheme(scheme: String) = {
      val result = u.clone
      result.setScheme(scheme)
      result
    }

    def newRawPath(p: String) = {
      val result = u.clone
      result.setRawPath(p)
      result
    }

    def notNullPathParts = {
      val pathParts = Option(u.getPathParts) map { _.asScala.toList }
      (pathParts | List.empty) filter { s => s != null && s.length > 0 }
    }
  }
}

trait HttpRequestWrapper {
  import HttpRequestWrapper._

  def getParameter(s: String): Option[String]
  def getScheme = Option(getRequestGenericUrl.getScheme) map (_.toLowerCase)
  def getSessionAttribute[T](s: String): EarlyReturn \/ T
  def scheme: HttpRequestType = getScheme match {
    case Some("http")  => Http
    case Some("https") => Https
    case None          => Missing
    case _             => Other
  }
  def getRequestURI: String
  def getRequestGenericUrl: GenericUrl = new GenericUrl(getRequestURI)
  def getHostname = Option(getRequestGenericUrl.getHost) | ""
}

object HttpRequestWrapper {
  sealed abstract class HttpRequestType
  case object Http extends HttpRequestType
  case object Https extends HttpRequestType
  case object Missing extends HttpRequestType
  case object Other extends HttpRequestType

  //  abstract sealed class TypedOptionalSessionAttributeResult[T]
  //  case class Success[T](x: T) extends TypedOptionalSessionAttributeResult[T]
  //  case class MissingAttribute[T](attrName: String) extends TypedOptionalSessionAttributeResult[T]
  //  case class IncorrectType[T](attrName: String, result: AnyRef) extends TypedOptionalSessionAttributeResult[T]

  implicit class HttpServletRequestWrapper(r: HttpServletRequest) extends HttpRequestWrapper {
    def getParameter(s: String) = Option(r.getParameter(s))
    def getSessionAttribute[T](s: String): EarlyReturn \/ T =
      safelyCall(r.getSession.getAttribute(s))(
        asInstanceOfNotNull[T](_).right,
        NoSuchSessionAttribute(s).left,
        WrappedFailure(_).left)
    def getRequestURI = r.getRequestURL.toString
  }
}

trait NonInitializedFilter extends Filter {
  override def destroy(): Unit = ()
  override def init(x: javax.servlet.FilterConfig): Unit = ()
}

trait FilterScaffold[T] { self: ServerPlumbing with Filter =>
  import stateTypes._

  val filterImplementation: CombinedStateAndFailure[T]

  override def doFilter(req: ServletRequest, resp: ServletResponse, chain: FilterChain) =
    doFilterPlumbing(req, resp, chain, filterImplementation)
}

trait ServletScaffold[T] extends HttpServlet {
  self: ServerPlumbing =>

  import stateTypes._

  def defaultImplementation: CombinedStateAndFailure[T] =
    throw new RuntimeException("no implementation")
  def implementationOfGet: CombinedStateAndFailure[T] =
    defaultImplementation
  def implementationOfPost: CombinedStateAndFailure[T] =
    defaultImplementation

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) =
    doServerPlumbing(req, resp, implementationOfGet)
  override def doPost(req: HttpServletRequest, resp: HttpServletResponse) =
    doServerPlumbing(req, resp, implementationOfPost)
}

abstract class FilterInjectionShim[T](implicit val bindingModule: BindingModule) extends Filter with ServerPlumbing with FilterScaffold[String]
abstract class ServletInjectionShim[T](implicit val bindingModule: BindingModule) extends HttpServlet with ServerPlumbing with ServletScaffold[T]

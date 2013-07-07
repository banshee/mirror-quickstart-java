package com.seattleglassware

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import org.scalatest.FunSuite
import scala.collection.mutable.Stack
import com.google.common.collect._
import com.google.common.collect.Ranges
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scalaz._
import Scalaz._
import JavaInterop._
import scala.util.control.Exception._
import com.seattleglassware._
import com.escalatesoft.subcut.inject._
import com.escalatesoft.subcut.inject.NewBindingModule._
import BindingIdentifiers._
import com.google.api.client.extensions.appengine.http.UrlFetchTransport
import com.google.api.client.json.jackson.JacksonFactory
import com.google.api.client.auth.oauth2.MemoryCredentialStore
import com.google.api.client.auth.oauth2.CredentialStore
import com.seattleglassware.EitherTWithState._
import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.auth.oauth2.Credential.AccessMethod
import javax.servlet.http.HttpServletRequest
import org.scalatest.mock.MockitoSugar
import com.google.api.client.http.GenericUrl
import scala.PartialFunction._
import GlasswareTypes._

@RunWith(classOf[JUnitRunner])
class ServerTests extends FunSuite with ShouldMatchers with MockitoSugar {
  test("safelyCall can handle null") {
    val result = safelyCall(returnsNull)(
      returnedValid = identity,
      returnedNull = returns1,
      threwException = turnsExceptionInto2)
    assert(result == "1")
  }

  test("safelyCall can create \\/ results") {
    val nullresult = safelyCall(returnsNull)(
      returnedValid = _.right,
      returnedNull = "gotnull".left,
      threwException = t => "gotexception".left)
    nullresult should be("gotnull".left)
  }

  test("safelyCall can handle a valid result") {
    val result = safelyCall(returnsString)(
      returnedValid = identity,
      returnedNull = returns1,
      threwException = turnsExceptionInto2)
    assert(result == "str")
  }

  test("safelyCall can handle an exception") {
    val result = safelyCall(throwsSomething)(
      returnedValid = identity,
      returnedNull = returns1,
      threwException = turnsExceptionInto2)
    assert(result == "2")
  }

  test("wrapping strings with exception handlers") {
    implicit class Exwrap(t: => String) {
      def wrapped = catching(classOf[Throwable]).withApply(t => "gotexception") {
        t
      }
    }
    def justThrowsAnException: String = throw new RuntimeException("snark")
    val s = justThrowsAnException.wrapped
    s should be("gotexception")
    ("just a string").wrapped should be("just a string")
  }

  def returnsNull: String = null
  def returnsString: String = "str"
  def throwsSomething: String = throw new RuntimeException("bang")
  def returns1() = "1"
  def turnsExceptionInto2(t: Throwable) = "2"
}

@RunWith(classOf[JUnitRunner])
class AuthUtilTests extends FunSuite with ShouldMatchers with MockitoSugar {
  test("can create an AuthUtil instance") {
    implicit val bindingmodule = TestBindings.configuration
    val a = new AuthUtil()
    val cf = a.newAuthorizationCodeFlow
  }

  test("urlPathMatch should match") {
    implicit val bindingmodule = TestBindings.configuration
    val a = new AuthFilterImplementation()
    val r = new TestHttpRequestWrapper("http://example.com/oauth2callback")
    val result = a.urlPathMatches { case "oauth2callback" :: Nil => true }(r)
    result should be(true)
  }

  test("AuthFilter should redirect to https if the hostname is appspot") {
    implicit val bindingmodule = TestBindings.configuration
    val a = new AuthFilterImplementation()
    val r = new TestHttpRequestWrapper("http://example.com/fnord")
    val (GlasswareState(_, effects), result) = a.authenticationCheck.run(new GlasswareState(r))
    cond(result) {
      case -\/(ExecuteRedirect(_)) => true
    } should be === (true)
    cond(effects) {
      case SetRedirectTo(_, _) :: t => true
    } should be === true
  }
}

class TestHttpRequestWrapper(url: String = "http://example.com/") extends HttpRequestWrapper {
  import stateTypes._

  val items = Map("attachment" -> "atch", "timelineItem" -> "tli", "user_id" -> "one")
  def getParameter(s: String) = items.get(s)
  def getSessionAttribute[T](s: String): EarlyReturn \/ T = NoSuchParameter(s).left
  def getRequestURI: String = url
}

@RunWith(classOf[JUnitRunner])
class AttachmentProxyServletTests extends FunSuite with ShouldMatchers {
  test("can run AttachmentProxyServlet") {
    implicit val testSpecificWithAuthorizedUser = newBindingModule { module =>
      import module._
      val credentialStore = new CredentialStore {
        /**
         * As seen from anonymous class $anon, the missing signatures are as follows.
         *  *  For convenience, these are usable as stub implementations.
         */
        def delete(x$1: String, x$2: com.google.api.client.auth.oauth2.Credential): Unit = ???
        def load(x$1: String, x$2: com.google.api.client.auth.oauth2.Credential): Boolean = true
        def store(x$1: String, x$2: com.google.api.client.auth.oauth2.Credential): Unit = ()
      }
      bind[CredentialStore] toSingle credentialStore
    } ~ TestBindings.configuration

    val a = new AttachmentProxyServletSupport()
    val b = a.attachmentProxyAction.run(GlasswareState(new TestHttpRequestWrapper))
    println("asdfff" + b.toString)
  }
}

object TestBindings {
  val testSpecific = newBindingModule { module =>
    import module._
    bind[String] idBy OAuthPropertiesFileLocation toSingle "web/oauth.properties"
    bind[CredentialStore] toSingle (new MemoryCredentialStore)
  }

  class TestCredential extends Credential(null.asInstanceOf[AccessMethod])

  val testSpecificWithAuthorizedUser = newBindingModule { module =>
    import module._
    val credentialStore = new MemoryCredentialStore
    bind[CredentialStore] toSingle credentialStore
  }

  implicit val configuration = testSpecific ~ UniversalBindings.configuration
  implicit val configurationWithAuthorizedTestUser = testSpecificWithAuthorizedUser ~ testSpecific ~ UniversalBindings.configuration
}

object StateProblem {
  case class MyStateType
  case class MyRightType
  case class MyLeftType

  type StateWithFixedStateType[+A] = State[MyStateType, A]
  type EitherTWithFailureType[F[+_], A] = EitherT[F, MyLeftType, A]
  type CombinedStateAndFailure[A] = EitherTWithFailureType[StateWithFixedStateType, A]

  def doSomething: CombinedStateAndFailure[MyRightType] = {
    val x = State[MyStateType, MyLeftType \/ MyRightType] {
      case s => (s, MyRightType().right)
    }
    EitherT[StateWithFixedStateType, MyLeftType, MyRightType](x)
  }

  val comprehension = for {
    a <- doSomething
    b <- doSomething
  } yield (a, b)

  implicit val feedTheCompiler = new Monoid[MyLeftType] {
    override def zero = MyLeftType()
    override def append(a: MyLeftType, b: => MyLeftType) = a
  }

  val otherComprehension = for {
    // this gets a compile error:
    // could not find implicit value for parameter M: scalaz.Monoid[com.seattleglassware.StateProblem.MyLeftType]
    (x, y) <- comprehension
    z <- doSomething
  } yield (x, y, z)

  // 
  val otherComprehensionWithWorkaround = for {
    xandy <- comprehension
    (x, y) = xandy
    z <- doSomething
  } yield (x, y, z)
}

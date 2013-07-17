package com.seattleglassware

import scala.PartialFunction.cond
import scala.util.control.Exception.catching
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar
import com.escalatesoft.subcut.inject.NewBindingModule.newBindingModule
import com.escalatesoft.subcut.inject.bindingIdToString
import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.auth.oauth2.Credential.AccessMethod
import com.google.api.client.auth.oauth2.CredentialStore
import com.google.api.client.auth.oauth2.MemoryCredentialStore
import BindingIdentifiers.OAuthPropertiesFileLocation
import GlasswareTypes._
import JavaInterop.safelyCall
import scalaz._
import scalaz.{ -\/ => -\/ }
import scalaz.Scalaz._
import scalaz.State
import scalaz.{ \/ => \/ }
import com.escalatesoft.subcut.inject.BindingModule
import org.scalatest.Matchers

@RunWith(classOf[JUnitRunner])
class ServerTests extends FunSuite with Matchers with MockitoSugar {
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
class AuthUtilTests extends FunSuite with Matchers with MockitoSugar {
  test("urlPathMatch should match") {
  }

  test("AuthFilter should redirect to https if the hostname is appspot") {
    implicit val bindingmodule = TestBindings.configuration
    val a = new AuthFilterSupport()
    val r = new TestHttpRequestWrapper("http://example.com/fnord")
    val (GlasswareState(_, effects), result) = a.authenticationCheck.run(new GlasswareState(r))
    cond(result) {
      case -\/(ExecuteRedirect(_, _)) => true
    } should be (true)
  }
}

@RunWith(classOf[JUnitRunner])
class AuthServletSupportTest extends FunSuite with Matchers with MockitoSugar {
  test("can finish oauth2 dance") {
    // Don't have a good test for this now since it hits google servers
    implicit val tbindings = TestBindings.configurationWithAuthorizedTestUser
    val auth = new AuthServletSupport
    val q = for {
      s <- auth.finishOAuth2Dance("code")
    } yield ()
    val (state, result) = q.run.run(TestBindings.defaultEmptyGlasswareState)
    cond(result) {
      case -\/(WrappedFailure(_, _)) => true
    } should be(true)
  }
}

class TestHttpRequestWrapper(url: String = "http://example.com/") extends HttpRequestWrapper {
  import stateTypes._

  val items = Map("attachment" -> "atch", "timelineItem" -> "tli", "user_id" -> "one", "userId" -> "two")
  def getParameter(s: String) = items.get(s)
  def getSessionAttribute[T](s: String): EarlyReturn \/ T =
    items.get(s).get.asInstanceOf[T].right[EarlyReturn]
  def getRequestURI: String = url
}

class TestStatefulParameterOperations extends FunSuite with Matchers with MockitoSugar {
  import stateTypes._

  class TestClassForState(implicit val bindingModule: BindingModule) extends StatefulParameterOperations {
    def failingShark: CombinedStateAndFailure[Int] = for {
      x <- pushComment("shark")
      _ <- FailedCondition("something").liftState
    } yield 1

    def succeedingShark: CombinedStateAndFailure[Int] = for {
      x <- pushComment("shark")
    } yield 2

    def failThenSucceed = for {
      a <- failingShark
      b <- succeedingShark
    } yield 3

    def succeedingBait: CombinedStateAndFailure[Int] = for {
      _ <- pushComment("bait")
    } yield 2

    def transformingSharkToBait = for {
      x <- transformLeft(failingShark)(x => succeedingBait)
    } yield x

    def failingBait: CombinedStateAndFailure[Int] = for {
      _ <- pushComment("bait")
      _ <- YieldToNextFilter.liftState
    } yield 2
  }

  test("fail followed by succeed sets state correctly") {
    val tc = new TestClassForState()(TestBindings.configuration)
    val (GlasswareState(_, items), result) = tc.failThenSucceed.run(TestBindings.defaultEmptyGlasswareState)
    items should be(List("shark") map Comment)
    cond(result) {
      case -\/(FailedCondition("something")) => true
    } should be(true)
  }

  test("fail followed by a transform sets state correctly") {
    val tc = new TestClassForState()(TestBindings.configuration)
    val (GlasswareState(_, items), result) = tc.transformingSharkToBait.run(TestBindings.defaultEmptyGlasswareState)
    items should be(List("bait", "shark") map Comment)
    cond(result) {
      case \/-(2) => true
    } should be(true)
  }

  test("can set user id") {
    val tc = new TestClassForState()(TestBindings.configuration)
    val (GlasswareState(_, items), result) = tc.setUserId("myuid").run(TestBindings.defaultEmptyGlasswareState)
    items should be(List(SetSessionAttribute("userId", "myuid")))
    cond(result) {
      case \/-(_) => true
    } should be(true)
  }
}

@RunWith(classOf[JUnitRunner])
class AttachmentProxyServletTests extends FunSuite with Matchers {
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
    val (state, result) = a.attachmentProxyAction.run(GlasswareState(new TestHttpRequestWrapper))
    cond(result) {
      case -\/(WrappedFailure(_, _)) => true
    } should be(true)
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

  def defaultEmptyGlasswareState = GlasswareState(new TestHttpRequestWrapper)
}

@RunWith(classOf[JUnitRunner])
class JavaInteropTests extends FunSuite with Matchers {
}

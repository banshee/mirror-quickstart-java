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
import com.seattleglassware.StateStuff._

@RunWith(classOf[JUnitRunner])
class ServerTests extends FunSuite with ShouldMatchers {
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

  test("lenses") {
    case class Fnord(x: Foo)
    case class Foo(y: String)
    val f = Lens.lensg[Fnord, Foo](set = fn => str => Fnord(str), get = fn => fn.x)
    val u = Lens.lensg[Foo, String](set = fn => str => Foo(str), get = fn => fn.y)
    val v = f >=> u
    val fn = Fnord(Foo("a"))
    val r = v.get(fn)
    var r1 = v.set(fn, "flurb")
    r1 should be(Fnord(Foo("flurb")))
  }

  def returnsNull: String = null
  def returnsString: String = "str"
  def throwsSomething: String = throw new RuntimeException("bang")
  def returns1() = "1"
  def turnsExceptionInto2(t: Throwable) = "2"
}

@RunWith(classOf[JUnitRunner])
class AuthUtilTests extends FunSuite with ShouldMatchers {
  test("can create an AuthUtil instance") {
    implicit val bindingmodule = TestBindings.configuration
    val a = new AuthUtil()
    val cf = a.newAuthorizationCodeFlow
    cf should be('right)
  }
}

@RunWith(classOf[JUnitRunner])
class AttachmentProxyServletTests extends FunSuite with ShouldMatchers {
  object EmptyHttpRequestWrapper extends HttpRequestWrapper {
    val items = Map("attachment" -> "atch", "timelineItem" -> "tli", "user_id" -> "one")
    def getParameter(s: String) = items.get(s)
    def getScheme = "http".some
    def getSessionAttribute[T](s: String): EarlyReturn \/ T = NoSuchParameter(s).left
  }

  test("can run AttachmentProxyServlet") {
    implicit val bindingmodule = TestBindings.configuration
    val a = new AttachmentProxyServletSupport()
    val b = a.attachmentProxyAction.run(GlasswareState(EmptyHttpRequestWrapper))
    println(b.toString)
  }
}

object UniversalBindings {
  implicit val configuration = newBindingModule { module =>
    import module._ // can now use bind directly
    bind[UrlFetchTransport] toSingle (new UrlFetchTransport)
    bind[JacksonFactory] toSingle (new JacksonFactory)
    //    bind[Z] toProvider { codeToGetInstanceOfZ() }
    //    bind[A] toProvider { implicit module => new AnotherInjectedClass(param1, param2) } // module singleton
    //    bind[B] to newInstanceOf[Fred] // create a new instance of Fred every time - Fred require injection
    //    bind[C] to moduleInstanceOf[Jane] // create a module scoped singleton Jane that will be used
    //    bind[Int] idBy PoolSize to 3 // bind an Int identified by PoolSize to constant 3
    //    bind[String] idBy ServerURL to "http://escalatesoft.com"
  }
}

object TestBindings {
  val testSpecific = newBindingModule { module =>
    module.bind[String] idBy OAuthPropertiesFileLocation toSingle "/Users/james/workspace/keys/mirror-quickstart-java/oauth.properties"
    module.bind[CredentialStore] toSingle (new MemoryCredentialStore)
  }

  implicit val configuration = testSpecific ~ UniversalBindings.configuration
}

object ProjectConfiguration {
  implicit val configuration = TestBindings.configuration
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
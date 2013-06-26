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

@RunWith(classOf[JUnitRunner])
class ServerTests extends FunSuite with ShouldMatchers {
  def returnsNull: String = null
  def returnsString: String = "str"
  def throwsSomething: String = throw new RuntimeException("bang")
  def returns1() = "1"
  def turnsExceptionInto2(t: Throwable) = "2"
  import JavaInterop._

  test("safelyCall can handle null") {
    val result = safelyCall(returnsNull)(returnedValid = identity, returnedNull = returns1, threwException = turnsExceptionInto2)
    assert(result == "1")
  }

  test("safelyCall can handle a valid result") {
    val result = safelyCall(returnsString)(returnedValid = x => x, returnedNull = returns1, threwException = turnsExceptionInto2)
    assert(result == "str")
  }

  test("safelyCall can handle an exception") {
    val result = safelyCall(throwsSomething)(returnedValid = x => x, returnedNull = returns1, threwException = turnsExceptionInto2)
    assert(result == "2")
  }

  class Animal
  class Cat extends Animal
  class Dog extends Animal

  test("safelyCall will call the right method if an object of an unexpected type is returned") {
    def returnsCat(): AnyRef = new Cat
    def returnsActualCat() = new Cat
    def returnsDog(): AnyRef = new Dog();
    {
      val result = safelyCall[String \/ Cat](returnsCat().asInstanceOf[Cat].right)(returnedValid = identity, returnedNull = "null".left, threwException = x => "exception".left)
      assert(result.isRight)
    }
    {
      val result = safelyCall[String \/ Cat](returnsDog().asInstanceOf[Cat].right)(returnedValid = identity, returnedNull = "null".left, threwException = x => "exception".left)
      result.fold(x => x, x => "incorrect") should be ("exception")
    }
    {
      def gotNPE(t: Throwable) = t match {
        case t: NullPointerException => "npe"
        case t                       => "other"
      }
      val result = safelyCall[String \/ Cat](castNoNull[Cat](null).right)(returnedValid = identity, returnedNull = "null".left, threwException = x => gotNPE(x).left)
      result.fold(x => x, x => "incorrect") should be ("npe")
    }
    {
      val gotNPE: Function[Throwable, String] = {
        case t: NullPointerException => "npe"
        case t                       => "other"
      }
      val result = safelyCall[String \/ Cat](null)(returnedValid = identity, returnedNull = "null".left, threwException = x => gotNPE(x).left)
      result.fold(x => x, x => "incorrect") should be ("null")
    }
  }
}
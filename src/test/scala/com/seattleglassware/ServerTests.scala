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

  def returnsNull: String = null
  def returnsString: String = "str"
  def throwsSomething: String = throw new RuntimeException("bang")
  def returns1() = "1"
  def turnsExceptionInto2(t: Throwable) = "2"
}
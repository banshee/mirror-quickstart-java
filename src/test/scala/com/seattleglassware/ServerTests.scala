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

@RunWith(classOf[JUnitRunner])
class ServerTests extends FunSuite with ShouldMatchers {
  test("safelyCall can handle null") {
    val result = safelyCall(returnsNull)(
        returnedValid = identity, 
        returnedNull = returns1, 
        threwException = turnsExceptionInto2)
    assert(result == "1")
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

  def returnsNull: String = null
  def returnsString: String = "str"
  def throwsSomething: String = throw new RuntimeException("bang")
  def returns1() = "1"
  def turnsExceptionInto2(t: Throwable) = "2"
}
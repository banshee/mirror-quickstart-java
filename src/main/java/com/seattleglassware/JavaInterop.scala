package com.seattleglassware

import scalaz._
import Scalaz._

object JavaInterop {
  private def identical[T](x: T): T = x
  def safelyCall[T](x: => T)(returnedValid: T => T, returnedNull: => T, threwException: Throwable => T): T =
    try {
      println(s"testing val $x");
      x match {
        case null => returnedNull
        case x    => returnedValid(x)
      }
    } catch {
      case t: Throwable => threwException(t)
    }

  def castNoNull[T](x: AnyRef) = x match {
    case null => throw new NullPointerException
    case x    => x.asInstanceOf[T]
  }
}
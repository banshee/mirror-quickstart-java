package com.seattleglassware

import scalaz._
import Scalaz._

object JavaInterop {
  private def identical[T](x: T): T = x
  def safelyCall[T, U](x: => U)(returnedValid: U => T, returnedNull: => T, threwException: Throwable => T): T =
    try {
      x match {
        case null => returnedNull
        case x    => returnedValid(x)
      }
    } catch {
      case t: Throwable => threwException(t)
    }

  def asInstanceOfNotNull[T](x: AnyRef) = x match {
    case null => throw new NullPointerException
    case x    => x.asInstanceOf[T]
  }

  implicit class AddsAsInstanceOfNotNull[T](x: AnyRef) {
    def asInstanceOfNotNull[T](x: AnyRef) = JavaInterop.asInstanceOfNotNull[T](x)
  }
}
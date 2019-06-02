package com.jcc

import scala.math.{abs, signum}

abstract class FutureValue(val currentValue: Double, val rate: Double) extends Ordered[FutureValue] {
  def value: Double
  def compare(that: FutureValue): Int = {
    val delta: Double = this.value - that.value
    (signum(delta) * abs(delta)).toInt
  }
}

class OneYear(currentValue: Double, rate: Double) extends FutureValue(currentValue, rate) {
  def value: Double = currentValue * scala.math.pow(1 + rate, 365)
}

object App3456 {
  val standard: OneYear = new OneYear(100, 0.05)
  (1 to 10).map(_ / 100.0).map(new OneYear(100, _)).count(_ < standard)
}

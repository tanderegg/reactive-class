package calculator

import math.pow
import math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal[Double](pow(b(), 2) - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
      Signal[Set[Double]](delta() match {
        case x if x > 0 => Set((-b() + sqrt(x)) / (2.0*a()), (-b() - sqrt(x)) / (2.0*a()))
        case _ => Set()
      })
  }
}

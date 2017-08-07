package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]
                  ): Signal[Double] = {
    Signal {
      Math.pow(b(), 2) - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]
                      ): Signal[Set[Double]] = {

    Signal {
      val delta = computeDelta(a, b, c)()
      if (delta < 0) Set(0)
      else {
        val bv = b()
        val av = a()
        Set(
          (-bv + Math.sqrt(delta)) / 2 * av,
          (-bv - Math.sqrt(delta)) / 2 * av
        )
      }
    }
  }
}

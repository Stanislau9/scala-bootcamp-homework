package basics

object basics {

  def gcd(a: Int, b: Int): Int = (math.abs(a), math.abs(b)) match {
    case (0, _) => math.abs(b)
    case (_, 0) => math.abs(a)
    case (aa, ba) if a > b => gcd(ba, aa % ba)
    case (aa, ba) => gcd(aa, ba % aa)
  }

  def lcm(a: Int, b: Int): Int = math.abs(a * b) / gcd(a, b)

}

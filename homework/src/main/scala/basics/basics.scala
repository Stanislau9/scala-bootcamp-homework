package basics

import scala.annotation.tailrec
import scala.math.abs

object basics {

  @tailrec
  def gcd(a: Int, b: Int): Option[Int] = (abs(a), abs(b)) match {
    case (0, 0) => None
    case (0, ba)             => Some(ba)
    case (aa, 0)             => Some(aa)
    case (aa, ba) if a > b  => gcd(ba, aa % ba)
    case (aa, ba)           => gcd(aa, ba % aa)
  }

  def lcm(a: Int, b: Int): Option[Int] = gcd(a, b).map(gcd => abs(a * b) / gcd)

}

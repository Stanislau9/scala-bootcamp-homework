package Collections

class Collections {
  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scanLeft(0)((a, b) => a + b).tail
  }

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    nums.take(n).zip(nums.drop(n)).flatMap(a => List(a._1, a._2))
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).max
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies.map(c => if (c + extraCandies >= candies.max) true else false)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val xPoints: Array[Int] = points.map(_(0)).sorted
    xPoints.zip(xPoints.tail).map(x => x._2 - x._1).max
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(s: String): Int = {
    var result: Int = 0
    s.foldLeft(0) { (acc, ch) =>
      if (ch == '(') acc + 1
      else if (acc > result) {
        result = acc
        acc
      } else if (ch == ')') acc - 1
      else
        acc
    }
    result
  }

  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  def balancedStringSplit(s: String): Int = {
    s.scanLeft(0)((i, ch) => if (ch == 'R') i + 1 else i - 1).count(_ == 0) - 1
  }

  // https://leetcode.com/problems/matrix-block-sum/
  def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = {
    val (x, y) = (mat.length - 1, mat(0).length - 1)
    val answer =
      for {
        i <- 0 to x
        j <- 0 to y
        sum = (for {
            r <- (if (i - K < 0) 0 else i - K) to (if (i + K > x) x else i + K)
            c <- (if (j - K < 0) 0 else j - K) to (if (j + K > y) y else j + K)
          } yield mat(r)(c)).sum
      } yield sum
    answer.toArray.grouped(y + 1).toArray
  }

  // file Collections exercises
  def findGap(l: List[Int]): Option[(Int, Int)] = {
    l match {
      case Nil    => None
      case _ :: _ => (l zip l.tail).find { case (one, two) => two - one > 1 }
    }
  }

  def min(list: List[Int]): Option[Int] = {
    list match {
      case Nil => None
      case _ =>
        Some(list.foldLeft(list.max) { (acc, x) => if (acc > x) x else acc })
    }
  }

  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = {
    list.foldLeft(List(zero)) { (acc, i) => f(acc.head, i) :: acc }.reverse
  }

  def count(s: String): List[(Char, Int)] = {
    s match {
      case _ if s.isEmpty => Nil
      case str =>
        str.head -> s.takeWhile(_ == str.head).count(_ == str.head) :: count(
          s.dropWhile(_ == str.head)
        )
    }
  }
}

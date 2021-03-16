package calculator

trait Result

object Result {
  final case class DivideResult(list: List[Double])  extends Result
  final case class SumResult(list: List[Double])     extends Result
  final case class AverageResult(list: List[Double]) extends Result
  final case class MinResult(list: List[Double])     extends Result
  final case class MaxResult(list: List[Double])     extends Result
}

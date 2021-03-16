package calculator

trait Command

object Command {
  final case class Divide(numbers: List[Double])  extends Command
  final case class Sum(numbers: List[Double])     extends Command
  final case class Average(numbers: List[Double]) extends Command
  final case class Min(numbers: List[Double])     extends Command
  final case class Max(numbers: List[Double])     extends Command
}

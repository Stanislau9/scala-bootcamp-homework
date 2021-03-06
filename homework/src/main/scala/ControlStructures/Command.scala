package ControlStructures

sealed trait Command

final case class Divide(dividend: Double, divisor: Double) extends Command

final case class Sum(numbers: List[Double]) extends Command

final case class Average(numbers: List[Double]) extends Command

final case class Min(numbers: List[Double]) extends Command

final case class Max(numbers: List[Double]) extends Command

final case class Result(command: Command, result: Double)
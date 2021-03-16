package calculator

import Command._
import Result._

object Solver {

  def parseCommand(inputString: String): Either[ErrorMessage, Command] = {
    inputString.toLowerCase.trim
      .replace(',', '.')
      .split("\\s+")
      .toList match {
      case _ :: Nil => Left(ErrorMessage("Error: no numbers"))
      case _ :: numbers if numbers.map(s => s.toDoubleOption).contains(None) =>
        Left(ErrorMessage("Error: invalid numbers"))
      case "divide" :: numbers  => Right(Divide(numbers.map(_.toDouble)))
      case "sum" :: numbers     => Right(Sum(numbers.map(_.toDouble)))
      case "average" :: numbers => Right(Average(numbers.map(_.toDouble)))
      case "min" :: numbers     => Right(Min(numbers.map(_.toDouble)))
      case "max" :: numbers     => Right(Max(numbers.map(_.toDouble)))
      case _                    => Left(ErrorMessage("Error: invalid command"))
    }
  }

  def calculate(command: Command): Either[ErrorMessage, Result] = command match {
    case Divide(numbers) =>
      if (numbers(1) == 0) Left(ErrorMessage("Error: divide by 0"))
      else Right(DivideResult(List(numbers.head / numbers(1), numbers.head, numbers(1))))
    case Sum(numbers)     => Right(SumResult(List(numbers.sum) ++ numbers))
    case Average(numbers) => Right(AverageResult(List(numbers.sum / numbers.length) ++ numbers))
    case Min(numbers)     => Right(MinResult(List(numbers.min) ++ numbers))
    case Max(numbers)     => Right(MaxResult(List(numbers.max) ++ numbers))
    case _                => Left(ErrorMessage("Error: invalid command"))
  }

  def renderResult(result: Result): Either[ErrorMessage, String] = result match {
    case DivideResult(list)  => Right(f"${list(1)} divided by ${list(2)} is ${list.head}")
    case SumResult(list)     => Right(f"the sum of ${list.tail.mkString(" ")} is ${list.head}")
    case AverageResult(list) => Right(f"the average of ${list.tail.mkString(" ")} is ${list.head}")
    case MinResult(list)     => Right(f"the minimum of ${list.tail.mkString(" ")} is ${list.head}")
    case MaxResult(list)     => Right(f"the maximum of ${list.tail.mkString(" ")} is ${list.head}")
    case _                   => Left(ErrorMessage("Error: render error"))
  }

}

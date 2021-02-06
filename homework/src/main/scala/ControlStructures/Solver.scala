package ControlStructures

object Solver {
  def numCheck(list: List[String]): Boolean =
    try {
      list.map(_.toDouble)
      false
    } catch {
      case _: NumberFormatException => true
    }

  def parseCommand(inputString: String): Either[ErrorMessage, Command] = {
    inputString.toLowerCase.trim
      .replace(',', '.')
      .split("\\s+")
      .toList match {
      case _ :: n if numCheck(n)    => Left(ErrorMessage("Error: invalid numbers"))
      case _ :: Nil                 => Left(ErrorMessage("Error: no numbers"))
      case "divide" :: x :: xx :: _ => Right(Divide(x.toDouble, xx.toDouble))
      case "sum" :: numbers         => Right(Sum(numbers.map(_.toDouble)))
      case "average" :: numbers     => Right(Average(numbers.map(_.toDouble)))
      case "min" :: numbers         => Right(Min(numbers.map(_.toDouble)))
      case "max" :: numbers         => Right(Max(numbers.map(_.toDouble)))
      case _                        => Left(ErrorMessage("Error: invalid command"))
    }
  }

  def calculate(command: Command): Either[ErrorMessage, Result] =
    command match {
      case c: Divide =>
        Either.cond(
          c.divisor != 0,
          Result(c, c.dividend / c.divisor),
          ErrorMessage("Error: divide by zero")
        )
      case c: Sum     => Right(Result(c, c.numbers.sum))
      case c: Average => Right(Result(c, c.numbers.sum / c.numbers.length))
      case c: Min     => Right(Result(c, c.numbers.min))
      case c: Max     => Right(Result(c, c.numbers.max))
      case _          => Left(ErrorMessage("Error: calculation mistake"))
    }

  def renderResult(result: Result): Either[ErrorMessage, String] = {
    result match {
      case Result(Divide(dividend, divisor), result) =>
        Right(f"${dividend} divided by ${divisor} is ${result}")
      case Result(Sum(numbers), result) =>
        Right(f"the sum of ${numbers.mkString(" ")} is ${result}")
      case Result(Average(numbers), result) =>
        Right(f"the average of ${numbers.mkString(" ")} is ${result}")
      case Result(Min(numbers), result) =>
        Right(f"the minimum of ${numbers.mkString(" ")} is ${result}")
      case Result(Max(numbers), result) =>
        Right(f"the maximum of ${numbers.mkString(" ")} is ${result}")
      case _ => Left(ErrorMessage("Error: render error"))

    }
  }
}

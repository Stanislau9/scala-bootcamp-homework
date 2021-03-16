package calculator

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import Solver._
import Command._
import Result._

class ParseCommandSpec extends AnyFlatSpec with Matchers {
  "   dIvIde 10,7 5" should "create Command Divide(10.7, 5)" in {
    parseCommand("   dIvIde 10,7 5") shouldEqual Right(Divide(List(10.7, 5)))
  }
  "suM    1 5   10  " should "create Command Sum(1, 5, 10)" in {
    parseCommand("suM    1 5   10  ") shouldEqual Right(Sum(List(1, 5, 10)))
  }
  "   Average 3    5    7,0 " should "create Command Average(3, 5, 7)" in {
    parseCommand("   Average 3    5    7,0 ") shouldEqual Right(Average(List(3, 5, 7)))
  }
  "min 4 -0 19" should "create Command Min(4,0,19)" in {
    parseCommand("min 4 -0 19") shouldEqual Right(Min(List(4, 0, 19)))
  }
  "max 1 -10 100" should "create Command Max(1, -10, 100)" in {
    parseCommand("max 1 -10 100") shouldEqual Right(Max(List(1, -10, 100)))
  }
  "max " should "create ErrorMessage Error: no numbers" in {
    parseCommand("max ") shouldEqual Left(ErrorMessage("Error: no numbers"))
  }
  "divide 3 d" should "create ErrorMessage Error: invalid numbers" in {
    parseCommand("divide 3 d") shouldEqual Left(ErrorMessage("Error: invalid numbers"))
  }
  "power 3 3" should "create ErrorMessage Error: invalid command" in {
    parseCommand("power 3 3") shouldEqual Left(ErrorMessage("Error: invalid command"))
  }
}

class CalculateSpec extends AnyFunSuite {
  test("Divide by 0 create ErrorMessage Error: divide by 0") {
    calculate(Divide(List(5, 0))) == Left(ErrorMessage("Error: divide by 0"))
  }
  test("Divide returns DivideResult") {
    calculate(Divide(List(4, 2))) == Right(DivideResult(List(2, 4, 2)))
  }
  test("Sum returns SumResult") {
    calculate(Sum(List(1, 2))) == Right(SumResult(List(3, 1, 2)))
  }
  test("Average returns AverageResult") {
    calculate(Average(List(3, 4, -1))) == Right(AverageResult(List(-1, 3, 4, -1)))
  }
  test("Min returns MinResult") {
    calculate(Min(List(0, 10, 1))) == Right(MinResult(List(0, 0, 10, 1)))
  }
  test("Max returns MaxResult") {
    calculate(Max(List(12, 1, 0))) == Right(MaxResult(List(12, 12, 1, 0)))
  }
  test("Returns error message: invalid command") {
    val otherCommand: Command = new Command {}
    calculate(otherCommand) == Left(ErrorMessage("Error: invalid command"))
  }
}

class RenderResultSpec extends AnyFreeSpec {
  "rendering answer" - {
    renderResult(DivideResult(List(2, 4, 2))) == Right("4 divided by 2 is 2")
    renderResult(SumResult(List(10, 3, 3, 3, 1))) == Right("the sum of 3 3 3 1 is 10")
    renderResult(AverageResult(List(3, 5, 2, 2))) == Right("the average of 5 2 2 is 3")
    renderResult(MinResult(List(1, 1, 5))) == Right("the minimum of 1 5 is 1")
    renderResult(MaxResult(List(5, 1, 5))) == Right("the maximum of 1 5 is 5")
  }
  "returns error message: render error" - {
    val otherResult: Result = new Result {}
    renderResult(otherResult) == Left(ErrorMessage("Error: render error"))
  }
}

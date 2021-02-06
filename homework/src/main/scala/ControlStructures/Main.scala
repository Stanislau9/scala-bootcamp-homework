package ControlStructures
import scala.io.Source
import Solver._

object Main {
  def main(args: Array[String]): Unit =
    Source.stdin.getLines() map process foreach println

  def process(x: String): String = {
    val result: Either[ErrorMessage, String] = for {
      command <- parseCommand(x)
      result  <- calculate(command)
      render  <- renderResult(result)
    } yield render

    result match {
      case Left(error)  => error.value
      case Right(value) => value
    }
  }
}

package calculator

import calculator.Main._
import org.scalatest.freespec.AnyFreeSpec

class ProcessSpec extends AnyFreeSpec {
  "solve" - {
    process("sum 2 2") == "the sum of 2 2 is 4"
  }
  "error" - {
    process("dfdfdfdfdf") == "Error: no numbers"
  }
}

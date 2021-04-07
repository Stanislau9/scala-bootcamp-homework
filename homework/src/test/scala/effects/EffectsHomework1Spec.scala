package effects

import effects.EffectsHomework1._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class EffectsHomework1Spec extends AnyFreeSpec with Matchers {
  "map" in {
    assert(IO("12").unsafeRunSync() == IO(12).map(_.toString).unsafeRunSync())
  }
  "flatMap" in {
    assert(IO("12").unsafeRunSync() == IO(12).flatMap(x => IO(x.toString)).unsafeRunSync())
  }
  "*>" in {
    var test = 0
    (IO { test = 10 } *> IO(12)).unsafeRunSync()
    assert(test == 10)
  }
  "as" in {
    var test = 0
    val res  = (IO { test = 10 } as 12).unsafeRunSync()
    assert(test == 10 && res == 12)
  }
  "void" in {
    assert(IO(12).void.unsafeRunSync() == ())
  }
  "attempt" - {
    "Right" in {
      assert(IO(12).attempt.unsafeRunSync() == Right(12))
    }
    "Left" in {
      val error = new java.lang.ArithmeticException("/ by zero")
      assert(IO(throw error).attempt.unsafeRunSync() == Left(error))
    }
  }
  "option" in {
    assert(IO(12).option.unsafeRunSync() == Some(12))
  }
  "handleErrorWith" - {
    "no error" in {
      assert(IO(1).handleErrorWith(_ => IO(12)).unsafeRunSync() == 1)
    }
    "error" in {
      assert(IO(1 / 0).handleErrorWith(_ => IO(12)).unsafeRunSync() == 12)
    }
  }
  "redeem" - {
    "no error" in {
      assert(IO(1).redeem(_ => 12, x => x + x).unsafeRunSync() == 2)
    }
    "error" in {
      assert(IO(1 / 0).redeem(_ => 12, x => x + x).unsafeRunSync() == 12)
    }
  }
  "redeemWith" - {
    "no error" in {
      assert(IO(1).redeemWith(_ => IO(12), x => IO(x + x)).unsafeRunSync() == 2)
    }
    "error" in {
      assert(IO(1 / 0).redeemWith(_ => IO(12), x => IO(x + x)).unsafeRunSync() == 12)
    }
  }
  "unsafeRunSync" in {
    assert(IO(1).unsafeRunSync() == 1)
  }
}

package effects

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object EffectsHomework1 {
  final class IO[A] private (run: () => A) {
    def map[B](f: A => B): IO[B]          = IO(f(run()))
    def flatMap[B](f: A => IO[B]): IO[B]  = IO(f(run()).unsafeRunSync())
    def *>[B](another: IO[B]): IO[B]      = flatMap(_ => another)
    def as[B](newValue: => B): IO[B]      = map(_ => newValue)
    def void: IO[Unit]                    = IO.unit
    def attempt: IO[Either[Throwable, A]] = IO(Try(run()).toEither)
    def option: IO[Option[A]]             = map(Option(_))
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = Try(run()) match {
      case Success(value)     => IO(value)
      case Failure(exception) => f(exception)
    }
    def redeem[B](recover: Throwable => B, map: A => B): IO[B]              = attempt.map(_.fold(recover, map))
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = attempt.flatMap(_.fold(recover, bind))
    def unsafeRunSync(): A                                                  = run()
    def unsafeToFuture()(implicit ec: ExecutionContext): Future[A]          = Future(run()) //?
  }

  object IO {
    def apply[A](body: => A): IO[A]        = new IO[A](() => body)
    def suspend[A](thunk: => IO[A]): IO[A] = thunk
    def delay[A](body: => A): IO[A]        = apply(body)
    def pure[A](a: A): IO[A]               = new IO[A](() => a)
    def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
      case Right(a)  => pure(a)
      case Left(err) => raiseError(err)
    }
    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
      case None        => raiseError(orElse)
      case Some(value) => pure(value)
    }
    def fromTry[A](t: Try[A]): IO[A] = t match {
      case Success(a)   => pure(a)
      case Failure(err) => raiseError(err)
    }
    def none[A]: IO[Option[A]]                                = pure(None)
    def raiseError[A](e: Throwable): IO[A]                    = new IO[A](() => throw e)
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) unit else IO.raiseError(e)
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit]   = if (cond) IO.raiseError(e) else unit
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) unit else action
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit]   = if (cond) action else unit
    val unit: IO[Unit]                                        = new IO(() => ())
  }
}

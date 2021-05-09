package effects

import cats.Monad
import cats.syntax.all._
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}

import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 */
object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_]: Clock: Monad, K, V](
      state: Ref[F, Map[K, (Long, V)]],
      expiresIn: FiniteDuration
  ) extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] =
      for {
        map   <- state.get
        value <- map.get(key).map { case (_, v) => v }.pure[F]
      } yield value

    def put(key: K, value: V): F[Unit] = {
      for {
        timestamp <- Clock[F].realTime(MILLISECONDS)
        _         <- state.getAndUpdate(map => map + (key -> (timestamp + expiresIn.toMillis, value)))
      } yield ()
    }

  }

  object Cache {
    def of[F[_]: Clock, K, V](
        expiresIn: FiniteDuration,
        checkOnExpirationsEvery: FiniteDuration
    )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {
      def expire(state: Ref[F, Map[K, (Long, V)]]): F[Unit] =
        for {
          _   <- T.sleep(checkOnExpirationsEvery)
          now <- Clock[F].realTime(MILLISECONDS)
          _ <- state.update { map =>
            map.filter { case (_, (timestamp, _)) => timestamp > now }
          }
        } yield ()

      for {
        state <- Ref.of[F, Map[K, (Long, V)]](Map.empty)
        cache = new RefCache[F, K, V](state, expiresIn)
        _ <- C.start(expire(state).foreverM.void)
      } yield cache
    }

  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _     <- cache.put(1, "Hello")
      _     <- cache.put(2, "World")
      _ <- cache
        .get(1)
        .flatMap(s =>
          IO {
            println(s"first key $s")
        })
      _ <- cache
        .get(2)
        .flatMap(s =>
          IO {
            println(s"second key $s")
        })
      _ <- IO.sleep(6.seconds)
      _ <- cache
        .get(1)
        .flatMap(s =>
          IO {
            println(s"first key $s")
        })
      _ <- cache
        .get(2)
        .flatMap(s =>
          IO {
            println(s"second key $s")
        })
      _ <- IO.sleep(12.seconds)
      _ <- cache
        .get(1)
        .flatMap(s =>
          IO {
            println(s"first key $s")
        })
      _ <- cache
        .get(2)
        .flatMap(s =>
          IO {
            println(s"second key $s")
        })
    } yield ExitCode.Success
  }
}

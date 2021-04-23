package http

import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.util.Random

object GuessServer extends IOApp {

  private val min: Int       = -1000
  private val max: Int       = 1000
  private val random: Random = new Random()
  private def NewGame(): (UUID, Int, Int, Int) =
    (UUID.randomUUID(), random.between(min, max), min, max)

  object Id    extends QueryParamDecoderMatcher[String]("id")
  object Guess extends QueryParamDecoderMatcher[Int]("guess")

  private def routes(ref: Ref[IO, Map[String, Int]]) = HttpRoutes.of[IO] {

    case GET -> Root / "start" =>
      val (id, number, minVal, maxVal) = NewGame()
      ref.update(_ + (id.toString -> number)) >> Ok(s"$id $minVal $maxVal")

//    case GET -> Root / "get" :? Id(id) =>
//      for {
//        number   <- ref.get.map(_.get(id))
//        response <- Ok(number.getOrElse(1234567890).toString)
//      } yield response

    case GET -> Root / "attempt" :? Id(id) :? Guess(guess) =>
      for {
        number <- ref.get.map(_.get(id))
        response <- {
          if (number.exists(_ > guess)) Ok("greater")
          else if (number.exists(_ < guess)) Ok("lower")
          else Ok("equal")
        }
      } yield response
  }

  override def run(args: List[String]): IO[ExitCode] =
    Ref.of[IO, Map[String, Int]](Map.empty).flatMap { ref =>
      BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9001, host = "localhost")
        .withHttpApp(routes(ref).orNotFound)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    }

}

object HttpClient extends IOApp {

  private val uri = uri"http://localhost:9001"

  def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      for {
        gameParams <- client
          .expect[String](uri / "start")
          .map(_.split(" "))
        (id, min, max) = (gameParams.head, gameParams(1).toInt, gameParams(2).toInt)
        result <- guessing(id, min, max, client)
        //_      <- IO(println(s"-------> $id    $min     $max  result $result"))
      } yield ()
    }.as(ExitCode.Success)

  private def guessing(id: String, min: Int, max: Int, client: Client[IO]): IO[Int] = {

    def attempt(min: Int, max: Int): Int = if ((max - min) == 1) max else (max + min) / 2

    def run(min: Int, max: Int): IO[Int] = {
      val g: Int = attempt(min, max)
      client.expect[String]((uri / "attempt").withQueryParams(Map("id" -> id, "guess" -> g.toString))).flatMap {
        case "equal"   => IO.pure(g)
        case "greater" => run(g, max)
        case "lower"   => run(min, g)
      }
    }

    run(min, max)
  }

}

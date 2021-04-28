package websocket

import cats.effect.{ExitCode, IO, IOApp, Resource}
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import fs2.Pipe
import fs2.concurrent.Queue
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import scala.concurrent.ExecutionContext
import scala.util.Random
import java.net.http.HttpClient

object GuessServer extends IOApp {

  private val min: Int       = -1000
  private val max: Int       = 1000
  private val random: Random = new Random()

  private def NewGame(): (Int, Int, Int) =
    (random.between(min, max), min, max)

  private val routes = HttpRoutes.of[IO] {

    case GET -> Root / "game" =>
      val (number, minVal, maxVal) = NewGame()
      val echoPipe: Pipe[IO, WebSocketFrame, WebSocketFrame] =
        _.collect {
          case WebSocketFrame.Text(message, _) if message == "game_params" =>
            WebSocketFrame.Text(s"$minVal $maxVal")
          case WebSocketFrame.Text(message, _) if message.toIntOption.exists(_ < number) =>
            WebSocketFrame.Text("greater")
          case WebSocketFrame.Text(message, _) if message.toIntOption.exists(_ > number) => WebSocketFrame.Text("lower")
          case WebSocketFrame.Text(message, _) if message.toIntOption.contains(number)   => WebSocketFrame.Text("equal")
        }

      for {
        queue <- Queue.bounded[IO, WebSocketFrame](10)
        response <- WebSocketBuilder[IO].build(
          receive = queue.enqueue,
          send = queue.dequeue.through(echoPipe),
        )
      } yield response

  }

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(routes.orNotFound)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

}

object WebSocketGuessClient extends IOApp {

  private def guessing(client: WSConnectionHighLevel[IO]): IO[String] = {

    val game_params: IO[List[Int]] = for {
      _ <- client.send(WSFrame.Text("game_params"))
      answer <- client.receiveStream.collectFirst {
        case WSFrame.Text(s, _) => s
      }.compile.string
    } yield answer.split(" ").map(_.toInt).toList

    val (min, max) = game_params.unsafeRunSync() match {
      case minVal :: maxVal :: Nil => (minVal, maxVal)
    }

    def attempt(min: Int, max: Int): Int = if ((max - min) == 1) max else (max + min) / 2

    def run(min: Int, max: Int): IO[String] = {

      val g: Int = attempt(min, max)

      for {
        _ <- client.send(WSFrame.Text(g.toString))
        answer <- client.receiveStream.collectFirst {
          case WSFrame.Text(s, _) => s
        }.compile.string
        result <- answer match {
          case s if s == "equal"   => IO.pure(g.toString)
          case s if s == "greater" => run(g, max)
          case s if s == "lower"   => run(min, g)
        }
      } yield result
    }

    run(min, max)
  }

  private val uri = uri"ws://localhost:9001/game"

  override def run(args: List[String]): IO[ExitCode] = {
    val clientResource: Resource[IO, WSConnectionHighLevel[IO]] = Resource
      .eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))

    clientResource.use { client =>
      for {
        res <- guessing(client)
        _   <- IO(println(s"result -> $res"))
      } yield ExitCode.Success
    }
  }
}

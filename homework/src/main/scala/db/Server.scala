package db

import db.authors.authorRoutes
import db.books.bookRoutes

import org.http4s.{Request, Response}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder

import cats.effect.{ConcurrentEffect, ExitCode, IO, Timer}
import cats.data.Kleisli
import cats.syntax.all._

import doobie.Transactor

import scala.concurrent.ExecutionContext

object Server {

  private def routes(transactor: Transactor[IO]): Kleisli[IO, Request[IO], Response[IO]] = {
    bookRoutes(transactor) <+> authorRoutes(transactor)
  }.orNotFound

  def start(transactor: Transactor[IO])(implicit concurrent: ConcurrentEffect[IO], timer: Timer[IO]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 8080, host = "localhost")
      .withHttpApp(routes(transactor))
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

}

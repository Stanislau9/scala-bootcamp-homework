package db

import db.DbCommon._

import cats.effect.{ExitCode, IO, IOApp}

import doobie._
import doobie.implicits._

object Main extends IOApp {

  private val ddl1: Fragment = Fragment.const(createTableAuthorsSql)
  private val ddl2: Fragment = Fragment.const(createTableBooksSql)
  private val dml: Fragment  = Fragment.const(populateDataSql)

  private def setup(): ConnectionIO[Unit] =
    for {
      _ <- ddl1.update.run
      _ <- ddl2.update.run
      _ <- dml.update.run
    } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    DbTransactor
      .pooled[IO]
      .use { transactor =>
        for {
          _ <- setup().transact(transactor)
          _ <- Server.start(transactor)
        } yield ()
      }
      .as(ExitCode.Success)

}

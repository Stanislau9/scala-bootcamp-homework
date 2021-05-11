package db

import db.DbCommon._
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import cats.effect.IO
import doobie._
import doobie.implicits._
import db.Instances._

import java.time.LocalDate
import java.util.UUID

object authors {

  def fetchAuthorByName(name: String): doobie.Query0[Author] =
    (author ++ fr"WHERE name = $name").query[Author]

  def fetchAuthorByBirthday(birthday: LocalDate): ConnectionIO[List[Author]] =
    (author ++ fr"WHERE birthday = $birthday").query[Author].stream.compile.toList

  def fetchAllAuthors: ConnectionIO[List[Author]] =
    author.query[Author].stream.compile.toList

  def addAuthor(name: String, birthday: LocalDate): doobie.ConnectionIO[Int] =
    fr"INSERT INTO authors (id, name, birthday) VALUES (${UUID
      .randomUUID()}, $name, $birthday)".update.run

  def updateAuthor(id: UUID, name: String, birthday: LocalDate): doobie.ConnectionIO[Int] =
    fr"UPDATE authors SET name = $name, birthday = $birthday  WHERE id = $id".update.run

  def authorRoutes(transactor: Transactor[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {

    case GET -> Root / "author" / "name" / name =>
      fetchAuthorByName(name).option
        .transact(transactor)
        .flatMap(author => Ok(author.getOrElse("not found").toString))

    case GET -> Root / "author" / "birthday" / birthday =>
      fetchAuthorByBirthday(LocalDate.parse(birthday)).transact(transactor).flatMap(authors => Ok(authors.toString))

    case GET -> Root / "author" / "all" =>
      fetchAllAuthors.transact(transactor).flatMap(authors => Ok(authors.toString))

    case value @ POST -> Root / "author" =>
      value
        .as[Author]
        .flatMap(
          a =>
            addAuthor(a.name, a.birthday)
              .transact(transactor)
              .flatMap(_ => Ok(s"added")))

    case value @ PUT -> Root / "author" =>
      value
        .as[Author]
        .flatMap(
          a =>
            updateAuthor(a.id, a.name, a.birthday)
              .transact(transactor)
              .flatMap(_ => Ok(s"updated")))

//    case value @ DELETE -> Root / "author" =>
//      value
//        .as[Author]
//        .flatMap(
//          a =>
//            updateAuthor(a.id, a.name, a.birthday)
//              .transact(transactor)
//              .flatMap(_ => Ok(s"deleted")))
  }

}

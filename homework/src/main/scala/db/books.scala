package db

import db.DbCommon._
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import cats.effect.IO
import doobie._
import doobie.implicits._
import db.Instances._

import java.time.Year
import java.util.UUID

object books {

  def fetchBookByName(name: String): doobie.Query0[BookWithAuthor] =
    (bookWithAuthor ++ fr"WHERE a.name = $name").query[BookWithAuthor]

  def fetchBookByYear(year: Int): ConnectionIO[List[Book]] =
    (book ++ fr"WHERE year = $year").query[Book].stream.compile.toList

  def fetchBook(title: String): doobie.Query0[Book] =
    (book ++ fr"WHERE title = $title").query[Book]

  def fetchAllBooks: ConnectionIO[List[Book]] =
    book.query[Book].stream.compile.toList

  def updateBook(id: UUID, title: String, year: Year): doobie.ConnectionIO[Int] =
    fr"UPDATE books SET title = $title, year = $year  WHERE id = $id".update.run

  def bookRoutes(transactor: Transactor[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {

    case GET -> Root / "book" / "title" / title =>
      fetchBook(title).option
        .transact(transactor)
        .flatMap(book => Ok(book.getOrElse("not found").toString))

    case GET -> Root / "book" / "author" / name =>
      fetchBookByName(name).option
        .transact(transactor)
        .flatMap(books => Ok(books.toString))

    case GET -> Root / "book" / "all" =>
      fetchAllBooks
        .transact(transactor)
        .flatMap(books => Ok(books.toString))

    case GET -> Root / "book" / "year" / year =>
      fetchBookByYear(year.toIntOption.getOrElse(-1))
        .transact(transactor)
        .flatMap(books => Ok(books.toString))

    case value @ PUT -> Root / "book" =>
      value
        .as[Book]
        .flatMap(
          b =>
            updateBook(b.id, b.title, b.year)
              .transact(transactor)
              .flatMap(_ => Ok(s"updated")))

  }

}

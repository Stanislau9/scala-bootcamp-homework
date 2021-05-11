package db

import cats.effect.IO
import doobie.Meta
import io.circe.generic.semiauto.deriveDecoder
import io.circe.Decoder
import org.http4s.EntityDecoder
import org.http4s.circe.jsonOf
import java.time.{LocalDate, Year}
import java.util.UUID

object Instances {
  implicit val uuidMeta: Meta[UUID]           = Meta[String].timap(UUID.fromString)(_.toString)
  implicit val yearMeta: Meta[Year]           = Meta[String].timap(Year.parse)(_.toString)
  implicit val localDateMeta: Meta[LocalDate] = Meta[String].timap(LocalDate.parse)(_.toString)

  implicit val authorDecoder: Decoder[Author]                 = deriveDecoder
  implicit val authorDecoderEntity: EntityDecoder[IO, Author] = jsonOf

  implicit val bookDecoder: Decoder[Book]                 = deriveDecoder
  implicit val bookDecoderEntity: EntityDecoder[IO, Book] = jsonOf
}

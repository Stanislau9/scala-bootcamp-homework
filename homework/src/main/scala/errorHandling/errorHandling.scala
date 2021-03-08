package errorHandling

import cats.data.ValidatedNec
import cats.syntax.all._

import java.time.YearMonth
import java.time.format.DateTimeFormatter
import scala.util.Try

object errorHandling {

  object Homework {

    case object Card {

      final case class Name(name: String) extends AnyVal

      final case class Number(number: Long) extends AnyVal

      final case class ExpirationDate(expirationDate: YearMonth) extends AnyVal

      final case class SecurityCode(securityCode: Int) extends AnyVal

    }

    import Card._

    case class PaymentCard(name: Name, number: Number, expirationDate: ExpirationDate, securityCode: SecurityCode)

    sealed trait ValidationError

    object ValidationError {

      final case object NameLengthIsInvalid extends ValidationError {
        override def toString: String = "Name must be between 3 and 30 characters"
      }

      final case object NameHasSpecialCharacters extends ValidationError {
        override def toString: String = "Name can't contains special symbols"
      }

      final case object InvalidNumber extends ValidationError {
        override def toString: String = "Card's number must contains 16 digits"
      }

      final case object ExpirationDateIsNotDate extends ValidationError {
        override def toString: String = "Expiration date must be in format mm/yy"
      }

      final case object ExpirationDateTooOld extends ValidationError {
        override def toString: String = "Expiration date is too old"
      }

      final case object InvalidSecurityCode extends ValidationError {
        override def toString: String = "Security code must contains 3 digits"
      }

    }

    object PaymentCardValidator {

      import ValidationError._

      type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

      private def validateNumber(number: String): AllErrorsOr[Number] = {
        if (number.toLongOption.isEmpty || number.length != 16) InvalidNumber.invalidNec
        else Number(number.toLong).validNec
      }

      private def validateName(name: String): AllErrorsOr[Name] = {

        def validateUsernameLength: AllErrorsOr[Name] = {
          if (name.length >= 3 && name.length <= 30) Name(name).validNec
          else NameLengthIsInvalid.invalidNec
        }

        def validateUsernameContents: AllErrorsOr[Name] = {
          if (name.matches("^[a-zA-Z]+$")) Name(name).validNec
          else NameHasSpecialCharacters.invalidNec
        }

        validateUsernameLength.productR(validateUsernameContents)
      }

      private def validateExpirationDate(expirationDate: String): AllErrorsOr[ExpirationDate] = {

        val date: Option[YearMonth] =
          Try(YearMonth.parse(expirationDate, DateTimeFormatter.ofPattern("MM/yy"))).toOption

        def validateExpirationDateIsDate: AllErrorsOr[ExpirationDate] = {
          if (date.isEmpty) ExpirationDateIsNotDate.invalidNec
          else ExpirationDate(date.get).validNec
        }

        def validateExpirationDateIsTooOld: AllErrorsOr[ExpirationDate] = {
          if (YearMonth.now().compareTo(date.get) > 0) ExpirationDateTooOld.invalidNec
          else ExpirationDate(date.get).validNec
        }

        validateExpirationDateIsDate.productR(validateExpirationDateIsTooOld)
      }

      private def validateSecurityCode(securityCode: String): AllErrorsOr[SecurityCode] = {
        if (securityCode.toIntOption.isEmpty || securityCode.length != 3) InvalidSecurityCode.invalidNec
        else SecurityCode(securityCode.toInt).validNec
      }

      def validate(
          name: String,
          number: String,
          expirationDate: String,
          securityCode: String
      ): AllErrorsOr[PaymentCard] =
        (validateName(name),
         validateNumber(number),
         validateExpirationDate(expirationDate),
         validateSecurityCode(securityCode)).mapN(PaymentCard)
    }

  }

}

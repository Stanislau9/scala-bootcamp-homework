package implicits

// make as many exercises as you can

object Task1 {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = (x: Money, y: Money) => x.amount.compare(y.amount)
}

object Task2 {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  implicit class toString[T](entity: T) {
    def show(implicit f: Show[T]): String = f.show(entity)
  }

  implicit val ShowUser: Show[User] = (user: User) => f"User ${user.name} with ID ${user.id}"

  User("1", "Valera").show
}

object Task3 {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  implicit class StringToUser[T](entity: String) {
    def parse(implicit f: Parse[T]): Either[Error, T] = f.parse(entity)
  }

  implicit val ShowUser: Parse[User] = (str: String) =>
    str.trim.split(' ').toList match {
      case id :: name :: Nil => Right(User(id, name))
      case _                 => Left("Not a user")
    }
}

object Task4 {
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`
  trait Equal[T] {
    def equal(x: T, y: T): Boolean
  }

  final case class User(id: String, name: String)

  implicit class EqualOrNotCompile[T](x: T) {
    def ===(y: T)(implicit f: Equal[T]): Boolean = f.equal(x, y)
  }

  implicit val IntEqual: Equal[Int] = (x: Int, y: Int) => x == y

}

object AdvancedHomework {
  trait Task[T[_]] {
    def flatMap[A, B](x: T[A])(f: A => T[B]): T[B]
  }
}

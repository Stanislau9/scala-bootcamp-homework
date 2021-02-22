package implicits

object TypeclassTask {

  trait HashCode[T] {
    def hash(t: T): Int
  }

  object HashCode {
    def apply[T](implicit instance: HashCode[T]): HashCode[T] = instance
  }

  implicit class HashCodeSyntax[A](x: A) {
    def hash(implicit hashCode: HashCode[A]): Int = hashCode.hash(x)
  }

  implicit val HashCodeString: HashCode[String] = (string: String) => string.hashCode
  "abc".hash
}

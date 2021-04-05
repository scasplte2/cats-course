package part5alien

import cats.Monoid

object ContravariantFunctors {

  trait Format[T] { // referred to as a contravariant typeclass (nothing to do with type variance)

    self =>

    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = (value: A) => self.format(func(value))
  }

  def format[A](value: A)(implicit f: Format[A]): String = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  // problem: given Format[MyType], can we havea Format[Option[MyType]]?
//  implicit def getOptionFormat[T](implicit f: Format[T]): Format[Option[T]] = new Format[Option[T]] {
//    override def format(value: Option[T]): String = f.format(value.get)
//  }
  import cats.instances.option._
implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] = f.contramap[Option[T]](_.getOrElse(m.empty))

//  def contramap[A, T](func: A => T)(implicit f: Format[T]): Format[A] = new Format[A] {
//    override def format(value: A): String = f.format(func(value))
//  }

  // composition of transformations happen in reverse
  // Map applies transformations in sequence
  // Contramap applies transformations in REVERSE sequence

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._ // implicit Show[Int]
  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  // extension methods
  import cats.syntax.contravariant._
  val showOptsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))


  def main(args: Array[String]): Unit = {
    println(format(Option(Option(42))))

  }
}

package part5alien

import cats.Monoid

object InvariantFunctors {

  trait Crypto[A] {
    self =>

    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))
      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](enc: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(enc)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)
    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  /*
  How can we support this type of logic for ints, double, Option[String]?
   */

  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)

  // todo - support Option[String]
  import cats.instances.option._
  import cats.instances.double._
  implicit val optionCrypto: Crypto[Option[String]] = caesarCypher.imap[Option[String]](_.getOrElse(""), Option(_))

  // todo - gneralize via implicit def, given a Crypto[T] => Cryto[Option[T]] if you have a Monoid[T] in scope
  implicit def getOptionCrypto[T](implicit crypto: Crypto[T], init: Monoid[T]): Crypto[Option[T]] =
    crypto.imap[Option[T]](_.getOrElse(init.empty), Option(_))


  import cats.Invariant
  import cats.Show
  import cats.instances.string._ // Show[String]
  val showString = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(Show[String])(Option(_))(_.getOrElse(""))

  import cats.syntax.invariant._
  val showOptionString2 = Show[String].imap(Option(_))(_.getOrElse(""))

  trait MyInvariant[F[_]] {
    def imap[A, B](fa: F[A])(forth: A => B)(back: B => A): F[B]
  }

  trait MyContraVariant[F[_]] extends MyInvariant[F] {
    def contramap[A, B](fa: F[A])(back: B => A): F[B]

    override def imap[A, B](fa: F[A])(forth: A => B)(back: B => A): F[B] = contramap(fa)(back)
  }

  trait MyFunctor[F[_]] extends MyInvariant[F] {
    def map[A, B](fa: F[A])(forth: A => B): F[B]

    override def imap[A, B](fa: F[A])(forth: A => B)(back: B => A): F[B] = map(fa)(forth)
  }


  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("lets encrypt")
    val decrypted = decrypt[String](encrypted)
    println(encrypted)
    println(decrypted)
    println(encrypt(Math.PI))
    println(decrypt[Double](encrypt(Math.PI)))
    val strOpt = Option("Hello")
    println(encrypt(strOpt))
    println(decrypt[String](encrypt(strOpt)))
    println(encrypt(Option(Math.PI)))
    println(decrypt[Option[Double]](encrypt(Option(Math.PI))))

  }


}

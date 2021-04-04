package part4typeClasses

object Applicatives {

  // applicatives = Functors + the pure method
  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val aList = Applicative[List].pure(2) // List(2)

  import cats.instances.option._
  val anOption = Applicative[Option].pure(2) // Some(2)

  // pure extension method
  import cats.syntax.applicative._
  val aSweetList = 2.pure[List]
  val aSweetOption = 2.pure[Option]

  // Validated does not obey monadic laws, but can be applicative
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(43)
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1)

  val validatedApplicative = Applicative[ErrorsOr]

  // todo: thought experiment
  //def ap[F[_], A, B](ff: F[A => B])(fa: F[A]): F[B] = ???
  def productWithApplicatives[F[_]: Applicative, A, B](fa: F[A], fb: F[B]): F[(A, B)] = {

    val functionWrapper: F[B => (A, B)] = Applicative[F].map(fa)(a => (b: B) => (a, b) /* function */)
    Applicative[F].ap(functionWrapper)(fb)

// my attempts
//    val apa = ap[F[_], A, (A, B)]((a: A) => Applicative[F].map(fa)(_))(fa)
//    val apb = ap[F[_], B, (A, B)]()(fb)
//
//
//
//
//    Applicative[F].map[(A, B), (A, B)](apa) {
//      a => Applicative[F].map[(A, B), (A, B)](apb) {
//        b =>
//      }
//    }
  }

  // Applicative have the above ap method ap[F[_], A, B](ff: F[A => B])(fa: F[A]): F[B]
  // Applicative can implement product from Semigroupals
  // => Applicatives extend Semigroupals

  def main(args: Array[String]): Unit = {

  }

}

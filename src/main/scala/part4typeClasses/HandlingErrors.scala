package part4typeClasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors {

  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](e: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
    def handleError[A](fa: F[A])(f: E => A): F[A] = handleErrorWith(fa) {
      //e => map(raiseError(e))(f)
      e => pure(f(e))
    }
  }

  trait MyMonadError[F[_], E] extends Monad[F] with MyApplicativeError[F, E] {
    //def raiseError[A](e: E): F[A]
    def ensure[A](fa: F[A])(error: E)(predicate: A => Boolean): F[A]
  }

  import cats.MonadError
  import cats.instances.either._ // implicit MonadError
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String]
  val success: ErrorOr[Int] = monadErrorEither.pure(23) // Right(32)
  val failure: Either[String, Int] = monadErrorEither.raiseError[Int]("oops")
  // recover
  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "oops" => 44
    case _ => 0
  }
  // recoverWith
  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "oops" => monadErrorEither.pure(32) // returns Erroror[Int]
    case _ => Left("something else") // returns ErrorOr[Int]
  }

  // "filter"
  val filteredSuccess = monadErrorEither.ensure(success)("number too small")(_ < 100)

  // Try and Future
  import cats.instances.try_._ // implicit MonadError[Try], E = Throwable
  val exception = new RuntimeException("really bad")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception) // Failure(exception)

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  MonadError[Future, Throwable].raiseError(exception) // Future which will complete with a Failure(exception)

  // applicatives => ApplicativeError
  import cats.ApplicativeError
  import cats.data.Validated
  import cats.instances.list._ // implicit Semigroup[List] => ApplicativeError[ErrorsOr, List[String]]
  type ErrorsOr[T] = Validated[List[String], T]
  val applErrorsVal = ApplicativeError[ErrorsOr, List[String]]
  // have access to pure, eaiseError, handleError, handleErrorWith

  // extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raiseError, handleError, handleErrorWith
  val extendedSuccess = 42.pure[ErrorsOr] // requires the implicit ApplicativeError[ErrorsOr, List[String]]
  val extendedError: ErrorsOr[Int] = List("badness").raiseError[ErrorsOr, Int]
  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 43
  }

  import cats.syntax.monadError._ // ensure method
  val testedSuccess = success.ensure("Something bad")( _ > 1)


  def main(args: Array[String]): Unit = {

  }

}

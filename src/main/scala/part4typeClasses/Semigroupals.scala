package part4typeClasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]
  val optSemigroupal = Semigroupal[Option]
  val aTupledOption = Semigroupal[Option].product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled = Semigroupal[Option].product(Some(123), None) // None

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture = Semigroupal[Future].product(Future("the meaning of life"), Future(42)) // Future(("the meaning of life", 42))

  import cats.instances.list._
  // actually outputs - List(1,a),(1,b),(2,a),(2,b))
  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b")) // guess - List(((1, 2), ("a", "b"))) or List(("1", "2", "a", "b"))

  // todo 1:
  import cats.Monad
  def productWithMonads[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    Monad[F].flatMap(fa) { a =>
      Monad[F].map(fb) {
        b => (a, b)
      }
    }

  import cats.syntax.functor._ // for .map
  import cats.syntax.flatMap._ // for .flatMap
  def productWithMonadsFor[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] = for {
    a <- fa
    b <- fb
  } yield (a, b)

  // MONADS EXTEND SEMIGROUPALS
  // example: Validated is not monadic but useful to combine
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires implicit Semigroup[List[_]]
  val invalidsCombination = Semigroupal[ErrorsOr].product(
    Validated.invalid(List("something wrong", "something wrong 2")),
    Validated.invalid(List("this isn't right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._
  val anEitherCombination = Semigroupal[EitherErrorsOr].product(
    Left(List("error 1", "error 2")),
    Left(List("error 3"))
  )

  // monadic associativity law - m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

  // todo 2: define a Semigroupal[List] which does a zip
  object Todo2 {
    object ZipListSemigroupal extends Semigroupal[List] {
      override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
    }

    val aTupledList2 = ZipListSemigroupal.product(List("a", "b"), List(1, 2))
  }

  def main(args: Array[String]): Unit = {
    println(aTupledList)
    println(invalidsCombination)
    println(anEitherCombination)
    println(Todo2.aTupledList2)
  }

}

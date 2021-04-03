package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numList = List(1, 2, 3)
  val charList = List('a', 'b', 'c')

  //todo 1.1: all the combinations of (number, char)?
  val combinedList: List[(Int, Char)] = numList.flatMap(n => charList.map(l => (n,l)))
  val forCombinedList: List[(Int, Char)] =
    for {
      n <- numList
      l <- charList
    } yield (n, l)

  // options
  val numberOption = Option(2)
  val charOption = Option('d')

  // todo 1.2: combination of (number and char)?
  val forCombinedOption: Option[(Int, Char)] =
    for {
      n <- numberOption
      c <- charOption
    } yield (n, c)

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(2)
  val charFuture = Future('a')

  // todo 1.3: combine
  val forCombinedFuture: Future[(Int, Char)] =
    for {
      n <- numberFuture
      c <- charFuture
    } yield (n, c)

  /**
   * Pattern
   *   - wrapping a value into a monadic value
   *   - the flatMap mechanism
   *
   *   MONADS (must be higher-kinded)
   */
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](value: M[A])(f: A => M[B]): M[B]
    def map[A, B](value: M[A])(f: A => B): M[B] =
      flatMap(value)(v => pure(f(v)))
  }

  import cats.Monad
  import cats.instances.option._ // option monad
  val optionMonad = Monad[Option]
  val anOption = Monad[Option].pure(3) // Option(4) == Some(4)
  val aTransformOption = Monad[Option].flatMap(anOption)(x => if ( x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._ // list monad
  val listMonad = Monad[List]
  val aList = Monad[List].pure(3)
  val aTransformedList = Monad[List].flatMap(aList)(x => List(x, x + 1))

  // todo 2: use a Monad[Future]
  import cats.instances.future._
  val aFuture = Monad[Future].pure(3) // requires and implicit ExecutionContext
  val aTransformedFuture = Monad[Future].flatMap(aFuture)(x => Future(x + 1))

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    for {
      n <- numbers
      c <- chars
    } yield (n, c)

  // you would need to repeat this API for every higher kinded type (Option, Future, etc.)
  // instead define the getPairs requiring a Monad
  def getPairMonad[A, B, F[_]: Monad](fa: F[A], fb: F[B]): F[(A, B)] =
    Monad[F].flatMap(fa) {
      a =>
        Monad[F].map(fb) {
          b => (a, b)
        }
    }

  // estension methods - weirder imports, pure & flatMap
  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  val oneOption = 1.pure[Option] // pulls implicit Monad[Option] => will resolve to Some(2)
  val oneList = 1.pure[List] // same as Monad[List].pure(1)

  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // todo 3: implement the map method in MyMonad
  // Monads extend Functors
  import cats.syntax.functor._
  val oneOptionMapped = Monad[Option].map(oneOption)(_ + 2)
  val oneOptionMapped2 = oneOption.map(_ + 2)

  // for-comprehensions
  val composedOption = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // todo 4: getPairs with for-comprehension
  def getPairMonadFor[A, B, F[_]: Monad](fa: F[A], fb: F[B]): F[(A, B)] = {
    for {
      a <- fa
      b <- fb
    } yield (a, b)
  }

  def main(args: Array[String]): Unit = {
    println(combinedList)
    println(forCombinedList)
    println(forCombinedOption)
    println(forCombinedFuture)
    println(aTransformOption)
    println(aTransformedFuture)
    println(getPairMonad(Option(3), Option('a')))
    println(getPairMonad(numList, charList))
    println(getPairMonad(numberOption, charOption))
    getPairMonad(numberFuture, charFuture).foreach(println)
    println(oneOptionMapped)
    println(oneOptionMapped2)
  }
}

package part4typeClasses

import cats.Applicative

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List("server1.jvm.com", "server2.jvm.com", "server3.jvm.com")
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 10)


  // solves a problem such as
  // - given a List[String]
  // - and a String => Future[Int]
  // - we want a Future[List{Int]] by acting on each string with the function

  // manual solution - involves unwrapping and rewrapping the futures though
  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (acc, host) =>
    val bwFuture: Future[Int] = getBandwidth(host)
    for {
      accBw <- acc
      bw <- bwFuture
    } yield accBw :+ bw
  }

  val allBWTraverse = Future.traverse(servers)(getBandwidth)
  val allBWSequence = Future.sequence(servers.map(getBandwidth))

  // todo 1
  import cats.syntax.applicative._
  import cats.syntax.apply._
  def listTraverse[F[_] : Applicative, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (acc, a) =>
      val fb: F[B] = f(a)
      (acc, fb).mapN(_ :+ _)

      // if you have flatMap and map
      //    for {
      //      acc2 <- acc
      //      b <- fb
      //    } yield acc2 :+ b
    }

  // todo 2: define sequnece
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = listTraverse(list)(identity)

  // todo 3
  import cats.instances.vector._
  // I think the ordering is going to be messed up but otherwise I am not sure
  // answer - you get the cartesian product of the combinations
  listSequence(List(Vector(1,2), Vector(3,4)))
  listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6)))

  import cats.instances.option._
  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse(list)(a => Some(a).filter(predicate))

  // todo 4 - what is the result of
  filterAsOption(List(2,4,6))(_ % 2 == 0) // Some(List(2,4,5))
  filterAsOption(List(1,2,3))(_ % 2 == 0) // None

  import cats.data.Validated
  import cats.instances.list._ // brings in Semigroup[List] => Applicative[ErrorsOr]
  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    }

  //output as above?
  val allTrueValidated = filterAsValidated(List(2,4,6))(_ % 2 == 0) // ErrorsOr[List(), List(2, 4, 6)]
  val someFalseValidation = filterAsValidated(List(1,2,3))(_ % 2 == 0) // ErrorsOr[List("predicated failed for 1", "&3"), List(2)]

  import cats.Foldable
  trait MyTraverse[L[_]] extends Foldable[L] {
    def traverse[F[_]: Applicative, A, B](list: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](list: L[F[A]]): F[L[A]]

    // hint
    type Identity[T] = T
    def map[A, B](la: L[A])(f: A => B): L[B] =
      traverse[Identity, A, B](la)(f)
  }

  import cats.Traverse
  import cats.instances.future._ // Applicative[Future]
  val allBWCats = Traverse[List].traverse(servers)(getBandwidth)

  // extension methods
  import cats.syntax.traverse._ // sequence and traverse methods
  val allBWCats2 = servers.traverse(getBandwidth)



  def main(args: Array[String]): Unit = {
    println(allTrueValidated)
    println(someFalseValidation)

  }

}

package part2abstractMath

import scala.util.Try

object Functors {

  // provides a map method
  val aModifiedList = List(1, 2, 3).map(_ + 1)
  val aModifiedOption = Option(2).map(_ + 3)
  val aModifiedTry = Try(42).map(_ + 1)

  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  // Cats Functor

  import cats.Functor
  import cats.instances.list._ // includes Functor[List]
  val listFunctor = Functor[List]
  val incremenetedNums = Functor[List].map(List(1, 2, 3))(_ + 1)

  import cats.instances.option._ //includes Functor[Option]
  val incrementedOption = Functor[Option].map(Option(2))(_ + 1)

  import cats.instances.try_._

  val incrementedTry = Functor[Try].map(Try(2))(_ + 1)

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = Functor[List].map(list)(_ * 10)

  def do10xFunctor[F[Int] : Functor](value: F[Int]): F[Int] = Functor[F].map(value)(_ * 10)

  def doWhatever[A, B, F[_] : Functor](value: F[A])(f: A => B): F[B] = Functor[F].map(value)(f)

  //todo 1: define your own functor for a binary tree
  // hint: no functor.instance, define an object which extends Functor[Tree]
  sealed trait Tree[+T]

  object Tree {
    def leaf[T](value: T): Tree[T] = Leaf(value)

    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }

  case class Leaf[+T](value: T) extends Tree[T]

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }

  val myTree = Tree.branch(
    3,
    Tree.branch(
      20,
      Tree.branch(
        300,
        Tree.leaf(4000),
        Tree.leaf(5000)),
      Tree.leaf(600)),
    Tree.leaf(70)
  )

  // estension method - map
  import cats.syntax.functor._
  val myIncTree = myTree.map(_ + 1)

  // todo 2: create a shorter version of do10x with extension
  def do10xFunc2[F[_]: Functor](value: F[Int]): F[Int] = value.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println(do10xFunctor(List(1, 2, 3)))
    println(do10xFunctor(Option(2)))
    println(do10xFunctor(myTree))
    println(myIncTree)
    println(do10xFunc2(myTree))

  }
}

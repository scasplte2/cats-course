package part2abstractMath

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad
  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None           => None
      case Some(Left(v))  => tailRecM(v)(f)
      case Some(Right(v)) => Option(v)
    }
  }

  // todo 1: define a monad for identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 2
  implicit object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x
    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v)  => tailRecM(v)(f)
      case Right(v) => v
    }
  }

  // harder example
  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // todo 3: define a monad for this tree
  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value) => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def traverseTree(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Right(value)) => Leaf(value)
        case Leaf(Left(value)) => traverseTree(f(value))
        case Branch(left, right) => Branch(traverseTree(left), traverseTree(right))
      }

      // todoList - nodes that need expanding
      // expanded - nodes that have been expanded
      // done - accumulator
      @tailrec
      def tailRec(todo: List[Tree[Either[A, B]]], expanded: Set[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] = {
        if (todo.isEmpty) done.head
        else todo.head match {
          case Leaf(Left(value)) => tailRec(f(value) :: todo.tail, expanded, done)
          case Leaf(Right(value)) => tailRec(todo.tail, expanded, Leaf(value) :: done)
          case node @ Branch(left, right) =>
            if (!expanded.contains(node)) tailRec(right :: left :: todo, expanded + node, done)
            else {
              val newLeft = done.head
              val newRight = done.tail.head
              val newBranch = Branch(newLeft, newRight)
              tailRec(todo.tail, expanded, newBranch :: done.drop(2))
            }
        }
      }

      tailRec(List(f(a)), Set(), List())
    }
  }


  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(10), Leaf(2))
    val changedTree = TreeMonad.flatMap(tree)(v => Branch(Leaf(v), Leaf(v * 10)))
    println(tree)
    println(changedTree)
  }

}

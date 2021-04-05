package part4typeClasses

import cats.{Eval, Monoid}

object Folding {

  // todo
  // implement everything in terms of foldLeft
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(List[B]()) {
      (a, acc) => f(a) :: acc
    }
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.foldLeft(List[B]()) {
      (acc, a) => acc.foldRight(f(a))(_ :: _)
    }
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = list.foldRight(List[A]()) { (a, acc) =>
      if (predicate(a)) a :: acc
      else acc
    }
    def combineAll[A: Monoid](list: List[A]): A = list.foldRight(Monoid[A].empty)(Monoid[A].combine)
  }

  import cats.Foldable
  import cats.instances.list._ // implicit Foldable
  val sum = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _)
  import cats.instances.option._
  val sumOption = Foldable[Option].foldLeft(Option(2), 30)(_ + _)

  // foldRight is stack-safe regardless of your container
  val sumRight = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) {
    (num, eval) => eval.map(_ + num)
  }

  // convenience methods
  import cats.instances.int._
  import cats.instances.string._
  val anotherSum = Foldable[List].combineAll(List(1, 2, 3))
  val mappedConcat = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // requires implicit Monad[String]

  import cats.instances.vector._
  val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

  // estension methods
  import cats.syntax.foldable._
  val sum3 = List(1, 2, 3).combineAll // req Foldable[List[, MonoidpInt]
  val mappedConcat2 = List(1, 2, 3).foldMap(_.toString) // req Monoid[String]

  def main(args: Array[String]): Unit = {
    import ListExercises._
    val numbers = (1 to 10).toList
    println(map(numbers)(_ + 1))
    println(flatMap(numbers)(a => (1 to a).toList))
    println(filter(numbers)(_ % 2 == 0))


    println(combineAll(numbers))

  }

}

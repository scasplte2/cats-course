package part3dataManipulation

object FunctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value
  // state = :iterative" computations

  // terrible, java like
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // pure FP with states

  // mistaken attempt
//  val fpComputation: State[Int, String] = State(
//    (currentCount: Int) => {
//      val addOne = currentCount + 1
//      (addOne, s"Added 1 to $currentCount, obtained $addOne")
//    }
//  ).map { c1 => {
//    val multFive = c1 * 5
//    (multFive, s"Multipled with 5, obtained $multFive")
//  }
//  }

  import cats.syntax.functor._
  val firstTransition = State((s: Int) => (s + 1, s"Added 1 to $s, obtained ${s + 1}"))
  val secondTransition = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}"))
  val fpComputation: State[Int, (String, String)] = for {
    s1 <- firstTransition
    s2 <- secondTransition
  } yield (s1, s2)

  // function composition is clunky
  val func1 = (s: Int) => (s + 1, "Added 1")
  val func2 = (s: Int) => (s * 5, "multi 5")
  val compositieFunc = func1.andThen {
    case (newState, firstResult) => (firstResult, func2(newState))
  }
  // *** results in burying the current state deeply inside of a bunch of tuples, so to chain we have to decompose each time
  // which is clunky, so we use the State typeclass to keep the current state at the top level and accumulate transition data

  // todo: an online store
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State((sc: ShoppingCart) => (ShoppingCart(item +: sc.items, price + sc.total), price + sc.total))
  def addToCart(items: List[String], price: Double): State[ShoppingCart, Double] =
    State((sc: ShoppingCart) => (ShoppingCart(items ++ sc.items, price + sc.total), price + sc.total))
  val jamesCart: State[ShoppingCart, Double] = for {
    _ <- addToCart("test1", 10.0)
    s2 <- addToCart(List("test2", "test3", "test4"), 55.38)
  } yield s2

  // todo 2: pure mental gynamstics
  // should return the state structure that will output f(a)
  def inspect[A, B](f: A => B): State[A, B] = State((s: A) => (s, f(s)))

  // returns a State data structure that, when run, returns the value of the state and makes no changes
  def get[A]: State[A, A] = State((s: A) => (s, s))

  // returns a State data structure that, when run, returns Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State((_: A) => (value, ()))

  // returns a State data structure that, when run, will return Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State((s: A) => (f(s), ()))

  // these methods are implemented in
  import cats.data.State._
  val program = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println(eleven)
    println(counted10)
    println(fpComputation.run(10).value)
    val g = addToCart("test1", 10.0)
    println(g.run(ShoppingCart(List(), 0)).value)
    println(jamesCart.run(ShoppingCart(List(), 0)).value)
    println(inspect[Int, Int](_ + 10).run(10).value)
    println(get.run(100).value)
    println(set(2).run(81830).value)
    println(modify[Int](_ * 5).run(42).value)
    println(program.run(2).value)
  }
}

package part2abstractMath

object Monoids {

  import cats.instances.int._
  import cats.syntax.semigroup._

  val nums = (1 to 1000).toList
  // |+| is associative, order doesn't matter
  val sumLeft = nums.foldLeft(0)(_ |+| _)
  val sumRight = nums.foldRight(0)(_ |+| _)

  // define a general api
  //def combineFold[T: Semigroup](list: List[T]): T = list.foldLeft()(_ |+| _) // what should be the starting value?
  // monoids define the empty value (zero value, neutral value)

  // MONOIDS
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999)
  val intEmpty = intMonoid.empty

  import cats.instances.option._ // construct implicit Monoid[Option[_]]
  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int])

  // todo 1: implement a combineFold
  def combineFold[T: Monoid](list: List[T]): T = list.foldLeft(Monoid[T].empty)(_ |+| _)

  // todo 2: combine a list of phonebooks as Maps[String, Int]
  import cats.instances.map._
  val phonebooks = List (
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 159,
      "Daniel" -> 889,
    ),
    Map(
      "Tina" -> 782
    )
  )

  val combinedPhonebooks: Map[String, Int] = combineFold(phonebooks)
  val em = Monoid[Map[String,Int]].empty

  // todo 3:
  case class ShoppingCart(items: List[String], total: Double)

  // Daniel's answer
//  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance(
//    ShoppingCart(List(), 0),
//    (sa, sb) => ShoppingCart(sa.items ++ sb.items, sa.total + sb.total)
//  )

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = new Monoid[ShoppingCart] {
    override def empty: ShoppingCart = ShoppingCart(List(), 0)

    override def combine(x: ShoppingCart, y: ShoppingCart): ShoppingCart = {
      val combCarts = x.items ++ y.items
      val newTotal = x.total + y.total
      ShoppingCart(combCarts, newTotal)
    }
  }

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  val aggregateCart = checkout(List(ShoppingCart(List("ball", "string"), 5.45), ShoppingCart(List("cup", "stick"), 1.99)))


  def main(args: Array[String]): Unit = {
    println(sumLeft == sumRight) // prints true
    println(combineOption)
    println(combinedPhonebooks)
    println(em)
    println(aggregateCart)
  }

}

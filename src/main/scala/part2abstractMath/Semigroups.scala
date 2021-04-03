package part2abstractMath

object Semigroups {

  // Semigroups combine elements of the same type
  import cats.Semigroup
  import cats.instances.int._
  import cats.instances.long._
  import cats.instances.string._
  import cats.syntax.semigroup._

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(1, 2) // addition

  val stringCombination = Semigroup[String].combine("hello ", "world!")

  def reduceInts(list: List[Int]): Int = list.reduce(Semigroup[Int].combine)
  def reduceString(list: List[String]): String = list.reduce(Semigroup[String].combine)

  // general API given a presence of a Semigroup
  def reduceThings[T: Semigroup](list: List[T]): T = list.reduce(Semigroup[T].combine)


  // todo 1: support a new type for the semigroup (create an implicit semiigroup)
  // hint - use the same pattern as the EQ pattern
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = (x: Expense, y: Expense) => Expense(Math.max(x.id, y.id), x.amount + y.amount)

  // extension methods from semigroup - |+|
  import cats.syntax.semigroup._
  val intSum = 1 |+| 2 // requires instance of implicit Semigroup[Int]
  val stringSum = "this is " |+| "great!" // Semigroup[String]
  val exSum = Expense(1L, 50) |+| Expense(5L, 23)

  // todo 2: implement reduceThings2
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)
    println("I love".combine(" cats"))

    val nums = (1 to 10).toList
    println(reduceInts(nums))
    println(reduceThings[Long](List(1L, 2L, 3L)))

    import cats.instances.option._
    val optNums: List[Option[Int]] = nums.map(Option(_))
    println(reduceThings(optNums))

    // use custom semigroup instance in list
    val expenses: List[Option[Expense]] = (1L to 10L).toList.map { id =>
      Option(Expense(id, 10*id))
    }
    println(reduceThings(expenses))
  }

}

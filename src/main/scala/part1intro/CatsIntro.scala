package part1intro

object CatsIntro {

  //Eq
 // val aComparison = 2 == "a string" // let the compiler fail this for you!!

  // part 1 - type class import
  import cats.Eq

  // part 2 - import TC instances for what we need
  import cats.instances.int._ // <- all the implicit type class instances for Int

  //part 2 - use the TC API
  val intEquality = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(2, 3)
  //val anUnsafeComparison = intEquality.eqv(2, "a string") -- doesn't compile!

  // part 4 - using extension methods
  import cats.syntax.eq._
  val anotherTypeSafeComp = 2 === 3 // === is equivalent to eqv
  val neqComparison = 2 =!= 3
  // val invalidComparison = 2 === "a string" -- fails again

  // part 5 - extend the TC operations to type constructores
  import cats.instances.list._
  val aListComparison = List(2) === List(3)

  // part 6 - custom types!
  import cats.instances.string._
  import cats.instances.double._

  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = (x: ToyCar, y: ToyCar) => x.model === y.model && x.price === y.price

  val toyCarComp = ToyCar("hot wheel", 3.50) === ToyCar("matchbox", 2.00)

}

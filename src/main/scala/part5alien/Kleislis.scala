package part5alien

object Kleislis {

  val func1: Int => Option[String] = x => if (x % 2 == 0) Some("is even") else None
  val func2: Int => Option[Int] = x => Some(x * 3)
  // func3 = func2 andThen func1

  // compose functions such as
  val plainFunc1: Int => String = x => if (x % 2 == 0) "is even" else "fail"
  val plainFunc2: Int => Int = x => x * 3
  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  import cats.data.Kleisli
  import cats.instances.option._ // FlatMap[Option]
  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K

  // convenience
  val multipl = func2K.map(_ * 2) // x => Option(...),map(_ * 2)
  val chain = func2K.flatMap(x => func1K)

  // todo - sounds like dependency injection
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  // *** this type is the same as Reader[A, B]
  // hint
  val times2 = Kleisli[Id, Int, Int](_ * 2)
  val plus4 = Kleisli[Id, Int, Int](_ + 4)
  val composed = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  val composedFor = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4


  def main(args: Array[String]): Unit = {

  }

}

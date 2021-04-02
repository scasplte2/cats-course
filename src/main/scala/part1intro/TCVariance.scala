package part1intro

object TCVariance {

  import cats.Eq
  import cats.instances.int._ // Eq[Int]
  import cats.instances.option._ // Eq[Option[_]]
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)
  //val anInvalidComparison = Some(2) === None // Need Eq[Some[Int]], Some[Int] != Option[Int] due to variance defintiion

  // variance
  class Animal
  class Cat extends Animal

  // covariant type: sub-type propogated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated BACKWARDS to the generic type
  // actions types, can DO something to the subtype
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  // rule of thumb: "HAS a T" = covariant, "ACTS on T" = contravariant
  // (perhaps, has or returns T => covariant, takes T as function input => contravariant)

  // contravariant TC
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("...")
  makeSound[Animal] //ok - Type class is defined above
  makeSound[Cat] // also ok - because contravariance means SM[Animal] <: SM[Cat] since Cat <: Animal and SM is contravariant (how does this "consume" though?

  // rule 1: contravariant TCs can use the supervclass instances if nothing is available strictly for that type
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // contravariant typoclass mean that you may not have to define an explicit defintion of the typeclass for a sub-type
  // that extends a super type because you can define the instance for the super class and use that due to contravariance

  // covariant TC
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show = "animal everywhere"
  }
  implicit object CatShow extends AnimalShow[Cat] {
    override def show: String = "so many cats"
  }
  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  // rule 2: covariant TCs will always use the more specific TC instance for that type
  // but may confuse the compiler if the general TC is also present

  // rule 3: you can't have both benefits
  // Cats uses INVARIANT type classes
  // smart constructors
  Option(2) === Option.empty[Int]

  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat])
    //println(organizeShow[Animal]) // <- this fails because there are two of them in scope since AnimalShow[Cat] <: AnimalShow[Animal]
  }

}

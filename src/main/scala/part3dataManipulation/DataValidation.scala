package part3dataManipulation

import cats.Semigroup

import scala.Right
import scala.util.Try

object DataValidation {

  import cats.data.Validated
  val aValidValue: Validated[String, Int] = Validated.valid(42) // "right" value
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "left" value
  val aTest: Validated[String, Int] = Validated.cond(true, 99, "it is smaller")

  // todo 1: use Either
  // test whether the number is a prime, non-negative, <= 100, and is even

  // incorrect first attempt
//  def testNumber(n: Int): Either[List[String], Int] = {
//    val b1 = ! ((2 until n-1) exists (n % _ == 0))
//    val b2 = n > 0
//    val b3 = n <= 100
//    val b4 = n % 2 == 0
//
//
//    val n1 = if (b1) Right(n) else Left(List("Not prime"))
//    val n2 = if (b1 && b2) Right(n) else n1 match {
//      case Left(err) => Left(err ++ List("Negative"))
//      case Right(_) => Left(List("Negative"))
//    }
//    val n3 = if (b1 && b2 && b3) Right(n) else n2 match {
//      case Left(err) => Left(err ++ List("Too big"))
//      case Right(_) => Left(List("Too big"))
//    }
//    val n4 = if (b1 && b2 && b3 && b4) Right(n) else n3 match {
//      case Left(err) => Left(err ++ List("Not even"))
//      case Right(_) => Left(List("Not even"))
//    }
//
//    n4
//
//  }

  // daniel's implementation
  def testNumber2(n: Int): Either[List[String], Int] = {
    val r1 = if (! ((2 until n-1) exists (n % _ == 0))) List() else List("Not prime")
    val r2 = if (n > 0) List() else List("Negative")
    val r3 = if (n <= 100) List() else List("Too big")
    val r4 = if (n % 2 == 0) List() else List("Not even")

    val combinedList = r1 ++ r2 ++ r3 ++ r4
    if (combinedList.isEmpty) Right(n)
    else Left(combinedList)
  }

  // can combine validated instances
  import cats.instances.list._
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)
  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated
      .cond(! ((2 until n-1) exists (n % _ == 0)), n, List("Not prime"))
      .combine(Validated.cond(n >= 0, n, List("Negative")))
      .combine(Validated.cond(n <= 100, n, List("Too big")))
      .combine(Validated.cond(n % 2 == 0, n, List("Not even")))

  // chain
  aValidValue.andThen(_ => anInvalidValue)
  // test a valid value with ensure
  aValidValue.ensure(List("something wrong"))(_ % 2 == 0)
  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)
  // plays nice with standard library
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present"))
  val tryToValidated: Validated[Throwable ,Int] = Validated.fromTry(Try("something".toInt))

  // going backwards
  aValidValue.toEither
  aValidValue.toOption

  // todo 1: form validation
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    // form must have keys name, email, password
    // ALL KEYS MUSTY be specified
    // name can't be blank
    // email ust contain "@"
    // password must be 10 or more characters

    implicit val combineFormString: Semigroup[String] = Semigroup.instance[String]((s1, s2) => s"$s1, $s2")

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"$fieldName must be provided"))

    val validatedName = (f: Map[String, String]) =>
      getValue(f, "name").ensure(List("Name cannot be blank"))(_.nonEmpty)

    val validatedEmail = (f: Map[String, String]) =>
      getValue(f, "email").ensure(List("Invalid email"))(_.contains('@'))

    val validatedPassword = (f: Map[String, String]) =>
      getValue(f, "password").ensure(List("Password must be 10 or more characters"))(_.length > 10)

    def validateForm(form: Map[String, String]): FormValidation[String] =
      validatedName(form)
        .combine(validatedEmail(form))
        .combine(validatedPassword(form))

    // daniels approach
    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"$fieldName may not be blank"))

    def properEmail(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("Email is invalid"))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("Password is too short"))

    def validateForm2(form: Map[String, String]): FormValidation[String] =
      getValue(form, "name").andThen(nonBlank(_, "name"))
        .combine { getValue(form, "email").andThen(properEmail) }
        .combine { getValue(form, "password").andThen(passwordCheck) }
        .map( _ => "Success")
  }

  // extension methods
  import cats.syntax.validated._
  val aValidMeanOfLike: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[List[String], Int] = List("Something went wrong").invalid[Int]


  def main(args: Array[String]): Unit = {
    println(testNumber2(200))
    println(validateNumber(-51))

    val form1 = Map(
      "name" -> "James",
      "email" -> "t@rtjvm.com",
      "password" -> "thisisalongpassword"
    )
    println(FormValidation.validateForm(form1))
    println(FormValidation.validateForm2(form1))
  }
}

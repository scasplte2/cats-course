package part4typeClasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives {

  trait MyApply[F[_]] extends Functor[F] with Semigroupal[F] {
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] // fundamental from F

    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      val fb2ab: F[B => (A, B)] = map(fa)((a: A) => (b: B) => (a, b))
      ap(fb2ab)(fb)
    }

    // todo
    def mapN[A, B, C](tuple: (F[A], F[B]))(f: (A, B) => C): F[C] = {
      // daniels implementation
      val tupleWrapper = product(tuple._1, tuple._2)
      map(tupleWrapper) {
        case (a, b) => f(a, b)
      }

      // my implementation
//      val fb2fc = map(tuple._1)((a: A) => (b: B) => f(a,b))
//      ap(fb2fc)(tuple._2)
    }

  }

  trait MyApplication[F[_]] extends MyApply [F] {
    def pure[A](x: A): F[A]
  }

  import cats.Apply
  import cats.instances.option._ // fetech Apply[Option]
  val applyOption = Apply[Option]
  val funcApp = Apply[Option].ap(Some((x: Int) => x + 1))(Some(2))

  import cats.syntax.apply._ // extension methods from Apply_
  val tupleOfOptions = (Option(1), Option(2))
  val optionOfTuple = tupleOfOptions.tupled // Some((1, 2, 3))
  val sumOption = tupleOfOptions.mapN(_ + _)


  // testing
  val jamesMapN = new MyApply[Option] {
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = Apply[Option].ap(ff)(fa)
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = Functor[Option].map(fa)(f)
    override def mapN[A, B, C](tuple: (Option[A], Option[B]))(f: (A, B) => C): Option[C] = {
      // my implementation
            val fb2fc = map(tuple._1)((a: A) => (b: B) => f(a,b))
            ap(fb2fc)(tuple._2)
    }
  }

  val danielsMapN = new MyApply[Option] {
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = Apply[Option].ap(ff)(fa)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = Functor[Option].map(fa)(f)

    override def mapN[A, B, C](tuple: (Option[A], Option[B]))(f: (A, B) => C): Option[C] = {
      // daniels implementation
      val tupleWrapper = product(tuple._1, tuple._2)
      map(tupleWrapper) {
        case (a, b) => f(a, b)
      }
    }
  }
  
  def main(args: Array[String]): Unit = {

    println(jamesMapN.mapN(tupleOfOptions)(_ + _))
    println(danielsMapN.mapN(tupleOfOptions)(_ + _))

  }
}

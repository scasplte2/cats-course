package part4typeClasses

import cats.{Applicative, Apply}

object WeakerMonads {

  trait MyFlatMap[F[_]] extends Apply[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    // todo
    // hint: Apply extends functor
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = flatMap(ff)(map(fa)(_))
    //                                                    |       |    \
    //                                                 F[A=>B]   F[A]  A=>B

    // Daniel gives a more verbose formulation
    //def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = flatMap(fa)(a => map(ff)(f => f(a)))
    ////                                                     |  \         /   \     \
    ////                                                  F[A]   A   F[A=>B]  A=>B  F[B]

  }

  trait MyMoand[F[_]] extends Applicative[F]  with MyFlatMap[F]{
    override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(pure(_))
  }

  import cats.FlatMap
  import cats.syntax.flatMap._ // flatMap extension method
  import cats.syntax.functor._ // map extension method

  def getPrais[F[_]: FlatMap, A, B](nums: F[A], chars: F[B]): F[(A, B)] = for {
    n <- nums
    c <- chars
  } yield (n, c)

  def main(args: Array[String]): Unit = {

  }

}

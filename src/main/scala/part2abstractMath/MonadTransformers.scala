package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.impl.Promise
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = values.map {
    case Some(value) => value
    case None => 0
  }.sum

  // option transformer
  import cats.data.OptionT
  import cats.instances.future._
  import cats.instances.list._ // fetch an implicit OptionT[List] which is a Monad[List]
  val listOfNumOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    num <- listOfNumOptions
  } yield (num, char)

  // either transformer
  import cats.data.EitherT
  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(42), Right(2)))
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))

  // todo 1:
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future("Server not found"))
    case Some(value) => EitherT.right(Future(value))
  }
  // Future[Either[String, Int]]

  // implement

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
    for {
      bw1 <- getBandwidth(s1)
      bw2 <- getBandwidth(s2)
    } yield bw1 + bw2 > 250
  // Future[Either[String, Boolean]]

    // todo 2:
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(value) => Left(s"Servers $s1 and $s2 cannot deal - reason: $value")
      case Right(false) => Left(s"Servers $s1 and $s2 cannot deal - reason: They ain't got the bandwidth!")
      case Right(true) =>  Right("Combination can withstand surge")
    }
  // Future[Either[String, String]]


  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
    getBandwidth("home").value.foreach(println)
    generateTrafficSpikeReport(bandwidths.head._1, bandwidths.tail.head._1).value.foreach(println)
  }

}

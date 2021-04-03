package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  import cats.instances.try_._
  val monadList = Monad[List] // summons out of the implicit scope
  val sSimpleList = Monad[List].pure(2)
  val aExtendedList = Monad[List].flatMap(sSimpleList)(x => List(x, x + 1))

  // applicable to Option, Try, Future
  // Either is also a monad
  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T] // Left "undesirable type" must be concrete!
  type ErrorOr[T] = Either[Throwable, T]
  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither = Monad[LoadingOr].pure(45)
  val aChangedLoading = Monad[LoadingOr].flatMap(anEither)(x => if (x % 2 == 0) Right(x + 1) else Left("Loading meaning of life"))

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet")
    else Right("Amsterdam")

  val orderId = 7091L
  val orderLocation = Monad[LoadingOr].flatMap[OrderStatus, String](getOrderStatus(orderId))(trackLocation)
  val orderLocation2 = for {
    o <- getOrderStatus(orderId)
  } yield trackLocation(o)

  // todo: the service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[F[_]] {
    def getConnection(cfg: Map[String, String]): F[Connection]
    def issueRequest(connection: Connection, payload: String): F[String]
  }

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def connectAndRequest[F[_]: Monad](httpService: HttpService[F])(cfg: Map[String, String], payload: String): F[String] =
    for {
      conn <- httpService.getConnection(cfg)
      res <- httpService.issueRequest(conn, payload)
    } yield res

  val httpOption = new HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length < 20) Some(s"request accepted: $payload") else None
  }


  val httpTry = new HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] =
      for {
        host <- if (cfg.contains("host")) Success(cfg("host")) else Failure(new Exception("missing key"))
        port <- if (cfg.contains("port")) Success(cfg("port")) else Failure(new Exception("missing key"))
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Try[String] =
      if (payload.length < 20) Success(s"request accepted: $payload") else Failure(new Error)
  }

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val httpFuture = new HttpService[Future] {
    override def getConnection(cfg: Map[String, String]): Future[Connection] =
      for {
        c <- Monad[Future].pure(cfg)
        host <- if (c.contains("host")) Future(c("host")) else throw new Exception("missing key")
        port <- if (c.contains("port")) Future(c("port")) else throw new Exception("missing key")
      } yield Connection(host, port)

      override def issueRequest(connection: Connection, payload: String): Future[String] =
        if (payload.length < 20) Future(s"request accepted: $payload") else throw new Error
    }


  val httpEither = new HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      for {
        c <- Right(cfg)
        host <- if (c.contains("host")) Right(c("host")) else Left(new Exception("missing key"))
        port <- if (c.contains("port")) Right(c("port")) else Left(new Exception("missing key"))
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length < 20) Right(s"request accepted: $payload") else Left(new Error)
  }


  def main(args: Array[String]): Unit = {
    println(aChangedLoading)

    val respOption = httpOption.getConnection(config).flatMap { conn =>
      httpOption.issueRequest(conn, "option payload")
    }
    println(respOption)

    val resTry = for {
      conn <- httpTry.getConnection(config)
      res <- httpTry.issueRequest(conn, "try payload")
    } yield res
    println(resTry)

    val resFuture = for {
      conn <- httpFuture.getConnection(config)
      res <- httpFuture.issueRequest(conn, "future payload")
    } yield res
    resFuture.onComplete(println)

    val resEither = for {
      conn <- httpEither.getConnection(config)
      res <- httpEither.issueRequest(conn, "Either payload")
    } yield res
    println(resEither)

    val resGeneric = connectAndRequest(httpTry)(config, "generic request")
    println(resGeneric)

  }
}

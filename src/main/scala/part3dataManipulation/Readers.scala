package part3dataManipulation

object Readers {

  /**
   * - configutation file -> initial data structure
   * - a DB layer
   * - an HTTP layer
   * - a business logic layer
   * */

  case class Configuration(dbUser: String, dbPass: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = s"dispatched $orderId" // select from db table and return status
    def getLastOrderId(username: String): Long = 452384
  }
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("Server started!")
  }

  // bootstrap
  val config = Configuration("James", "pass", "localhost", 1234, 8, "g@test.com")

  // using cats reader
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(cfg => DbConnection(cfg.dbUser, cfg.dbPass))
  val dbConn = dbReader.run(config)

  // Reader[I, O] but can use map to transform O
  val jamesOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbcon => dbcon.getOrderStatus(55))

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderIdReader = dbReader
      .map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    // identical to the one above
    val usersOrderFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    usersLastOrderIdReader.run(config)
    usersOrderFor.run(config)
  }

  // Pattern
  // 1. create an initial data structure
  // 2. create a reader which specifies how that data structure will be manipulated later
  // 3. then map and flatMap the reader to produce derived information
  // 4. when you need the final piece of information, you call run on the reader with the initial data structure


  // todo 1: email a user
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String): String = s"From: $emailReplyTo; to: $address >>> $contents"
  }

  def emailUser(username: String, userEmail: String): String = {
    // fetch the status of their last order
    // em,ail them with the email service, (content should contain order status)
    val emailService: Reader[Configuration, EmailService] = Reader(cfg => EmailService(cfg.emailReplyTo))

    val orderStatusEmail = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      email <- emailService.map(_.sendEmail(userEmail, s"Order status: $orderStatus"))
    } yield email

    orderStatusEmail.run(config)

  }

  // todo 2: what programming pattern do readers remind you of? (depedency injection)



  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("Jim"))
    println(emailUser("joe", "test@test.com"))
  }

}

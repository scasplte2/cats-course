package part3dataManipulation

object Writers {

  import cats.data.Writer
  // 1 - define writers at start of application
  val aWriter: Writer[List[String], Int] = Writer(List("Started"), 45)

  // 2 - manipulate them with pure FP
  val anIncremetedWrite: Writer[List[String], Int] = aWriter.map(_ + 1)
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting") // value stays the same but adds to logs
  val aWriterWithBoth = aWriter.bimap(_ :+ "Found something in bimap", _ + 1)
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something through mapBoth", value + 1)
  }

  // flatMap on Writers
  import cats.instances.vector._ // import a Semigroup[Vector]
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs
  import cats.instances.list._ // imports an implicit Monoid[List]
  val anEmptyWriter = aWriter.reset // clears the logs

  // 3 - dump either the value or lgos
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run

  // todo 1: rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting"), n)
    else countAndLog(n - 1).bimap(_ :+ n.toString, _ + n)
  }

  // todo 2:
  def naiveSum(n: Int): Int = {
    if(n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum at ${n - 1} = $lowerSum")
      lowerSum + n
    }
  }

  def writerSum(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else writerSum(n - 1).mapBoth { (logs, value) =>
      (logs :+ s"Computed sum at ${n - 1} = $value", value + n)
    }
  }

  // Daniel's implementation
  def sumWIthLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- sumWIthLogs(n - 1)
      _ <- Writer(Vector(s"Comuted sum at ${n - 1} = $lowerSum"), n)
    } yield lowerSum + n
  }

  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    countAndSay(10)
    countAndLog(10).written.foreach(println)
    naiveSum(5) // multiple threads will result in mixed output
    writerSum(5).written.foreach(println)
  }

}

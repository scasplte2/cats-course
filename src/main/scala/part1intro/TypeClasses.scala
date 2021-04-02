package part1intro

object TypeClasses {

  case class Person(name: String, age: Int)

  // part 1 - typeclass defintion
  trait JSONSerializer[T] {
    def toJSON(value: T): String
  }

  // part 2 - implicit instances (concrete instances of the typeclass)
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJSON(value: String): String = s""""$value""""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJSON(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJSON(value: Person): String =
      s"""
         | {"name": ${value.name}, "age": ${value.age}}
         |""".stripMargin.trim
  }

  //part 3 - define API
  def convertListToJSON[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(serializer.toJSON).mkString("[", ", ", "]")


  // part 4 - extends existing types
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit JSONSerializer: JSONSerializer[T]) {
      def toJSON: String = JSONSerializer.toJSON(value)
    }
  }

  def main(args: Array[String]): Unit = {

    import JSONSyntax._

    println(convertListToJSON(List(1, 2, 3)))
    println(convertListToJSON(List(Person("James", 32), Person("Alice", 23))))
    println(Person("Bob", 49).toJSON)
  }
}

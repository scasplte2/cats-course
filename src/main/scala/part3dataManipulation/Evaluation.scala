package part3dataManipulation

object Evaluation {

  // Cat distinguishes 1. eager evaluation, 2. evaluating lazily (recomputed every time), 3. eval lazily and keep (memoizing)
  import cats.Eval

  // eager evaluation
  val instantEval = Eval.now{
    println("Computing now")
    239484
  }

  // evaluated every time it is needed
  val lazyEval = Eval.always {
    println("lazy eval")
    3849
  }

  // computes when needed then remembers it
  val memoEval = Eval.later {
    println("memo eval")
    2743947
  }

  val composedEval = instantEval.flatMap(value1 => memoEval.map(value2 => value1 + value2))
  val composedAlwaysEval = for {
    v1 <- instantEval
    v2 <- lazyEval
  } yield v1 + v2

  // "remember" a computer value
  val dontRecompute = lazyEval.memoize

  val tutorial = Eval
    .always { println("Step 1..."); "put the guitar on your lap" }
    .map { step1 => println("Step 2"); s"$step1 then put your left hand on the neck" }
    .memoize // remember the value up to this point
    .map { step12 => println("Step 3, more complicated"); s"$step12 then with the right hand strike the strings" }

  // todo 2: implement such defer(Eval.now) does NOT run the side effects
  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)
    //eval.flatMap[T](Eval.later[T]) // <- my attempt

  // todo 3: rewrite the method with Evals
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else defer(reverseEval(list.tail).flatMap(lt => Eval.later { lt :+ list.head }))

  def reverseEvalFor[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else for {
      lt <- reverseEval(list.tail)
      el <- Eval.later { lt :+ list.head }
    } yield el


  def main(args: Array[String]): Unit = {
//    println(instantEval.value)
//    println(lazyEval.value)
//    println(lazyEval.value)
//    println(memoEval.value)
//    println(memoEval.value)
//    println(composedEval.value
//    println(composedEval.value)
//    println(composedAlwaysEval.value)
//    println(composedAlwaysEval.value)
//    println(tutorial.value)
//    println(tutorial.value)
//    val g = defer(Eval.now {
//      println("Now!")
//      42
//    })
//    println(g.value)

    println(reverseEvalFor((1 to 10000).toList).value)

  }
}

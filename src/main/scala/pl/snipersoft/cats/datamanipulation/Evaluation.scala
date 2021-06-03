package pl.snipersoft.cats.datamanipulation

import cats.Eval

object Evaluation extends App {
//  demonstration() //NPE!

  val instant: Eval[Int] = Eval.now { println("Computing (now)..."); 1 }
  val always: Eval[Int] = Eval.always { println("Computing (always)..."); 20 }
  val later: Eval[Int] = Eval.later { println("Computing (later)..."); 300 }
  val onlyOnce: Eval[Int] = always.memoize

//  demonstration()
//  combined()
//  memoize()
//  deferExercise()
  reverseListExercise()

  def demonstration(): Unit = {
    println("Start")
    println(instant.value)
    println(instant.value)
    println(always.value)
    println(always.value)
    println(later.value)
    println(later.value)
  }

  def combined(): Unit = {
    val combined = for {
      a <- later
      b <- always
      c <- instant
      d <- always
    } yield a + b + c + d

    //now
    println(combined.value) //later -> always -> always -> 341
    println(combined.value) //always -> always ->341
  }

  def memoize(): Unit = {
    val tutorial = Eval
      .always { println("step 1"); "step 1"}
      .map { s1 =>  println("step 2"); s1+" & step 2"}
      .memoize
      .map { s12 => println("step 3"); s12+" & step 3"}

    println(tutorial.value) //step 1 2 3
    println(tutorial.value) //step 3
  }

  def deferExercise(): Unit = {
    def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)

    defer(Eval.now { println("Now!"); 42}) //should not print
  }

  def reverseListExercise(): Unit = {
    def reverseList[T](list: List[T]): List[T] =
      if (list.isEmpty) list
      else reverseList(list.tail) :+ list.head

//    def reverseEval[T](list: List[T]): Eval[List[T]] =
//      if (list.isEmpty) Eval.now(list)
//      else reverseEval(list.tail).map(_ :+ list.head)

    def reverseEvalSafe[T](list: List[T]): Eval[List[T]] =
      if (list.isEmpty) Eval.now(list)
      else Eval.defer(reverseEvalSafe(list.tail).map(_ :+ list.head))

    println(reverseEvalSafe((1 to 10000).toList).value) //works! like tailRec!
  }
}

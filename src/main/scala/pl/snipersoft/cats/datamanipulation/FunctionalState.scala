package pl.snipersoft.cats.datamanipulation

import cats.data.State

//pure FP abstraction over "iterative" computations
//functions composition
//old state => (new state; result of computation)
object FunctionalState extends App {

  introduction()
  composition()
  shopExercise()
  mentalGymnasticExercise()

  def introduction(): Unit = {
    type MyState[S, A] = S => (S, A)

    val countAndSay: State[Int, String] = State(currentCount => (currentCount+1, s"Current count is $currentCount"))
    println(countAndSay.run(10).value) //Eval.value
  }

  def composition(): Unit = {
    val f1 = (s: Int) => (s+1, s"Added one to $s")
    val f2 = (s: Int) => (s*5, s"$s Multiplied five times")

    val first = State(f1)
    val second = State(f2)

    val composed = for {
      v1 <- first
      v2 <- second
    } yield (v1, v2)

    println(composed.run(1).value) //(10,(Added one to 1,2 Multiplied five times))

    //why to use state, not just functions?
    val composedFunction = f1.andThen {
      case (newState, firstResult) => (firstResult, f2(newState))
    }
    println(composedFunction(1)) //nested result! (Added one to 1,(10,2 Multiplied five times))
  }

  def shopExercise(): Unit = {
    case class ShoppingCart(items: List[String], total: Double)
    def addToCard(item: String, price: Double): State[ShoppingCart, Double] =
      State { cart => (ShoppingCart(items = item :: cart.items, total = cart.total+price), cart.total+price) }

    val cart = for {
      _ <- addToCard("CM.057", 1100)
      _ <- addToCard("PJ249", 1850)
      total <- addToCard("GR14", 2200)
    } yield total

    println(cart.run(ShoppingCart(List(), 0)).value)
  }

  def mentalGymnasticExercise(): Unit = {
//    def inspect[A, B](f: A => B): State[A, B] = State(a => (a, f(a)))
//    def get[A]: State[A, A] = State(a => (a, a))
//    def set[A](value: A): State[A, Unit] = State(_ => (value, ()))
//    def modify[A](f: A => A): State[A, Unit] = State(a => (f(a), ()))

    import cats.data.State._

    val program: State[Int, (Int, Int, Int)] = for {
      a <- get[Int]
      _ <- set[Int](a+10) // _ because of unit
      b <- get[Int]
      _ <- modify[Int](_+43)
      c <- inspect[Int, Int](_*2)
    } yield (a, b, c)
  }
}

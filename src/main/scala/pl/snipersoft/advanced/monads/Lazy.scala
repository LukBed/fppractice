package pl.snipersoft.advanced.monads


object Lazy {
  def apply[A](value: => A): Lazy[A] = new Lazy(value)
}

class Lazy[+A](value: => A)  {
  //call by need
  private lazy val internalValue = value

  def flatMap[B](f: (=>A) => Lazy[B]): Lazy[B] = f(internalValue)
  def get: A = internalValue
}

object LazyApp extends App {
  val firstLazy = Lazy {
    println("Computing first value") //print once
    42
  }

  val secondLazy = firstLazy.flatMap(nr => Lazy {
    println("Computing second value") //print once
    nr+5
  })

  println("Empty screen until now")

  assert(firstLazy.get == 42)
  assert(secondLazy.get == 47)
  secondLazy.get

  /*
  monad: unit, flatMap
  unit == apply
  left-identity: unit(x).flatMap(f) == f(x)
  right-identity: aMonadInstance.flatMap(unit) == aMonadInstance
  associativity: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
   */


}
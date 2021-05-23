package pl.snipersoft.advanced.typesystem

object Inheritance extends App {
//  mixing()
//  diamondProblem()
  superProblemAndTypeLinearization()

  def mixing(): Unit = {
    trait Writer[T]
    trait Closeable[T]
    trait GenericStream[T]
    def processStream[T](stream: GenericStream[T] with Writer[T] with Closeable[T]): Unit = ()
  }

  def diamondProblem(): Unit = {
    trait Animal { def name(): String }
    trait Lion extends Animal { override def name() = "lion" }
    trait Tiger extends Animal { override def name() = "tiger" }
    class Mutant extends Lion with Tiger
    val m = new Mutant
    println(m.name()) //tiger (last override)

    /*
    compiler way of thinking:
    class Mutant extends Lion with Tiger
    class Mutant extends Animal with { override def name() = "lion" } with Tiger
    class Mutant extends Animal with { override def name() = "lion" } with Animal with { override def name() = "tiger" }
     */
  }

  def superProblemAndTypeLinearization(): Unit = {
    trait Cold { def print(): Unit = println("cold") }
    trait Green extends Cold { override def print(): Unit = { println("green"); super.print() } }
    trait Blue extends Cold { override def print(): Unit = { println("blue"); super.print() } }
    class Red { def print(): Unit = { println("red"); } }
    class White extends Red with Green with Blue { override def print(): Unit = { println("white"); super.print() } }

    new White().print() // white blue green cold

    /*
    <?> - body of ?
    Cold = AnyRef with <Cold>
    Green = Cold with <Green> = AnyRef with <Cold> with <Green>
    Blue = Cold with <Blue> = AnyRef with <Cold> with <Blue>
    Red = AnyRef with <Red>
    White = Red with Green with Blue with <White>
          = AnyRef with <Red> with (AnyRef with <Cold> with <Green>) with (AnyRef with <Cold> with <Blue>) with <White>
          //remove redundant bodies which was earlier - type linearization
          = AnyRef with <Red> with <Cold> with <Green> with <Blue> with <White>
          //red body will not be executed because cold has not super.print
     */
  }
}

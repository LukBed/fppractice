package pl.snipersoft.advanced.typesystem

object StructuralTypes extends App {
  type JavaCloseable = java.io.Closeable

  introduction()
  typeRefinements()
  exercise()

  //it's based on reflection - low performance!

  def introduction() {
    class HipsterCloseable {
      def close(): Unit = println("I'm closing")
    }

    type UnifiedClosable = { //structural type
      def close(): Unit
    }

    def closeQuietly(closable: UnifiedClosable): Unit = closable.close()

    val javaCloseable: JavaCloseable = () => println("Java Closeable")
    val hipsterCloseable: HipsterCloseable = new HipsterCloseable
    closeQuietly(javaCloseable)
    closeQuietly(hipsterCloseable)
  }

  def typeRefinements() {
    type AdvancedCloseable = JavaCloseable {
      def closeSilently(): Unit
    }

    class AdvancedJavaCloseable extends JavaCloseable {
      override def close(): Unit = println("Java's closing")
      def closeSilently(): Unit = println("Java's closing silently")
    }

    def closeShh(c: AdvancedCloseable): Unit = c.closeSilently()

    closeShh(new AdvancedJavaCloseable)
//    closeShh(new HipsterCloseable) //nok - HipsterCloseable is not JavaCloseable

  }

  def structuralTypesAsStandaloneTypes(): Unit = {
    def closeMe(c: {def close(): Unit}): Unit = c.close()
  }


  def duckTyping(): Unit = {
    //static duck typing
    //something swims like a duck, and flies like a duck and looks like a duck - is duck
    type SoundMaker = { def makeSound(): Unit }
    class Dog { def makeSound(): Unit = "Hau!" }
    class Car { def makeSound(): Unit = "Wruuum!" }
    val sm: SoundMaker = new Dog
  }

  def exercise(): Unit = {
    //cons based list
    trait CBL[+T] {
      def head: T
      def tail: CBL[T]
    }

    class Human {
      def head: Brain = new Brain
    }

    class Brain {
      override def toString = "BRAINZ!"
    }

    def f[T](somethingWithHead: { def head: T}): Unit = println(somethingWithHead.head)

    val cbl: CBL[Int] = new CBL[Int] {
      override def head: Int = 0
      override def tail: CBL[Int]  = ???
    }

    f(new Human) // T = Brain
    f(cbl)

    object HeadEqualizer {
      type Headable[T] = { def head: T }
      def ===[T](a: Headable[T], b: Headable[T]): Boolean = a.head == b.head
    }

    HeadEqualizer.===(new Human, new Human)
    HeadEqualizer.===(cbl, cbl)
    HeadEqualizer.===(cbl, new Human) //not type safe due to reflection!
  }
}

package pl.snipersoft.advanced.typesystem

object PathDependentTypes extends App {

  def introduction(): Unit = {
    class Outer {
      class Inner
      object OuterObject
      type InnerType
      def print(i: Inner): Unit = println(i)
      def printGeneral(i: Outer#Inner): Unit = println(i)
    }

    def method(): Unit = {
      class Helper
      object HelperObject
      //    type MethodType //not compiling
      type MethodType = String
    }

    //per-instance
    val outer = new Outer
    val inner = new outer.Inner
    val secondOuter = new Outer
    outer.print(inner)
    //  val secondInner: secondOuter.Inner = new outer.Inner //not compiling - wrong context (path-dependent types)
    //  secondOuter.print(inner)
    secondOuter.printGeneral(inner) //supertype: Outer#Inner
  }

  def dbExercise(): Unit = {
//    trait Item[Key]
//    trait IntItem extends Item[Int]
//    trait StringItem extends Item[String]
//    get[IntItem](42) //should compile
//    get[StringItem]("abc") //should compile
//    get[IntItem]("abc") //should not compile

    trait Item[K] extends ItemLike {
      override type Key = K
    }
    trait IntItem extends Item[Int]
    trait StringItem extends Item[String]

    trait ItemLike {
      type Key
    }

   def get[ItemType <: ItemLike](key: ItemType#Key): ItemType = ???


    get[IntItem](42)
    get[StringItem]("abc")
//    get[IntItem]("abc") //should not compile
  }
}

package pl.snipersoft.advanced.typesystem

object TypeMembers extends App {

  class Animal
  class Cat extends Animal
  class Dog extends Animal

  def abstractTypeMembers() {
    class AnimalCollection {
      type AnimalType //abstract type members
      type BoundedAnimal <: Animal
      type SuperBoundedAnimal >: Dog <: Animal
      type AnimalCat = Cat //type alias
    }

    val ac = new AnimalCollection
    //  val dog: ac.AnimalType = new Dog //not compiling - it can be also crocodile
    val dog: ac.SuperBoundedAnimal = new Dog
    val cat: ac.AnimalCat = new Cat
  }

  def alternativeToGenerics() {
    trait MyList {
      type T

      def add(value: T): MyList
    }

    class IntList(value: Int) extends MyList {
      override type T = Int
      override def add(value: Int): MyList = ???
    }
  }

  def dotType(): Unit = {
    val cat = new Cat
    type CatType = cat.type
    val newCat: CatType = cat
//    val newCat: CatType = new Cat //not compiling
  }

  def typeApplicableToSomeTypesOnly(): Unit = {
    //don't change this trait (it's API)
    trait MyList {
      type A
      def head: A
      def tail: MyList
    }

    trait ApplicableToNumbers {
      type A <: Number
    }

    //code below should not compile - we accept only numbers
//    class StringList(h: String, t: StringList) extends MyList with ApplicableToNumbers {
//      override type A = String
//      override def head: String = h
//      override def tail: MyList = t
//    }

    //code below should compile
//    class IntList(h: Int, t: IntList) extends MyList with ApplicableToNumbers {
//      override type A = Int
//      override def head: Int = h
//      override def tail: MyList = t
//    }
  }
}

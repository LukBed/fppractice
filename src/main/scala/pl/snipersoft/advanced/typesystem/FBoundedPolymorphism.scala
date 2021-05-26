package pl.snipersoft.advanced.typesystem

//recursive Types and F-Bounded Polymorphism
object FBoundedPolymorphism extends App {

  traditionalWay()
  recursiveTypes()
  examples()
  recursiveTypesWithSelfTypes()
  typeClasses()
  pureTypeClass()

  def traditionalWay(): Unit = {
    trait Animal {
      def breed: List[Animal]
    }

    class Cat extends Animal {
      override def breed: List[Cat] = ??? //valid code
    }

    class Dog extends Animal {
      override def breed: List[Cat] = ??? //also valid code :(
    }
  }

  def recursiveTypes(): Unit = {
    trait Animal[A <: Animal[A]] { //recursive type: F-Bounded Polymorphism
      def breed: List[Animal[A]]
    }

    class Cat extends Animal[Cat] {
      override def breed: List[Animal[Cat]] = ???
    }

    class Dog extends Animal[Dog] {
      override def breed: List[Animal[Dog]] = ???
    }

    class Crocodile extends Animal[Dog] {
      override def breed: List[Animal[Dog]] = ??? //valid code :(
    }
  }

  def examples(): Unit = {
    //ORM
    trait Entity[E <: Entity[E]]

    //Java
    class Person extends Comparable[Person] {
      override def compareTo(o: Person): Int = ???
    }
  }

  def recursiveTypesWithSelfTypes(): Unit = {
    trait Animal[A <: Animal[A]] { self: A =>
      def breed: List[Animal[A]]
    }

    class Cat extends Animal[Cat] {
      override def breed: List[Animal[Cat]] = ???
    }

    class Dog extends Animal[Dog] {
      override def breed: List[Animal[Dog]] = ???
    }

    //invalid code
//    class Crocodile extends Animal[Dog] {
//      override def breed: List[Animal[Dog]] = ???
//    }

    //F-Bounded Polymorphism works only for one level of hierarchy
    trait Fish extends Animal[Fish]
    class Shark extends Fish {
      override def breed: List[Animal[Fish]] = List(new Code) ///problem again :(
    }
    class Code extends Fish {
      override def breed: List[Animal[Fish]] = ???
    }

  }

  def typeClasses(): Unit = {
    trait Animal //no method in Animal
    trait CanBreed[A] {
      def breed(a: A): List[A]
    }

    class Dog extends Animal
    object Dog {
      implicit object DogsCanBreed extends CanBreed[Dog] {
        override def breed(a: Dog): List[Dog] = List()
      }
    }

    implicit class CanBreedOps[A](animal: A) {
      def breed(implicit canBreed: CanBreed[A]): List[A] = canBreed.breed(animal)
    }

    new Dog().breed

    //wrong code
    class Cat extends Animal
    object Cat {
      implicit object CatsCanBreed extends CanBreed[Dog] {
        override def breed(a: Dog): List[Dog] = List()
      }
    }

//    new Cat().breed //not compiling, type class instance not found
  }

  def pureTypeClass(): Unit = {
    //simplified solution
    trait Animal[A] { //pure type classes
      def breed(a: A): List[A]
    }

    class Dog
    object Dog {
      implicit object DogAnimal extends Animal[Dog] {
        override def breed(a: Dog): List[Dog] = List()
      }
    }

    implicit class AnimalEnrichment[A](animal: A) {
      def breed(implicit animalTypeClassInstance: Animal[A]): List[A] = animalTypeClassInstance.breed(animal)
    }

    new Dog().breed
  }
}

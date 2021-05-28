package pl.snipersoft.cats.intro

object TypeClassesVariance extends App {

  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  val comparison = Option(2) === Option(3)
  //val invalidComparison = Some(2) === None //not compile
  Option(2) === Option.empty

  def variance(): Unit = {
    class Animal
    class Dog extends Animal

    //has a T
    class Cage[+T]
    val cage: Cage[Animal] = new Cage[Dog]

    //acts on T
    class Vet[-T]
    val vet: Vet[Dog] = new Vet[Animal] //what you can do for an animal, you can also do for a dog

    //variance affect how TC instances are being fetched
    trait SoundMaker[-T]
    implicit object AnimalSoundMaker extends SoundMaker[Animal]
    implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
    def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow")

    makeSound[Animal]
    makeSound[Dog]
    makeSound[Some[Int]]
    makeSound[Option[Int]]

    trait AnimalShow[+T] {
      def show: String
    }
    implicit object GeneralAnimalShow extends AnimalShow[Animal] {
      override def show: String = "Animals everywhere"
    }
    implicit object DogsShow extends AnimalShow[Dog] {
      override def show: String = "Dogs everywhere"
    }
    def organizeShow[T](implicit event: AnimalShow[T]): Unit = println(event.show)

    organizeShow[Dog]
    //organizeShow[Animal] //not compile - two instances available!

    //Cats uses invariant classes!
  }
}

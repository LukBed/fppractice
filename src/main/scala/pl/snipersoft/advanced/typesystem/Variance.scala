package pl.snipersoft.advanced.typesystem

object Variance extends App {

  trait Animal
  class Dog extends Animal
  class Cat extends Animal
  class Crocodile extends Animal
  class Kitty extends Cat

  def varianceTypes() {

    class CovariantCage[+T]
    val covariantCage: CovariantCage[Animal] = new CovariantCage[Cat]

    class InvariantCage[T]
    //  val invariantCage: InvarianceCage[Animal] = new InvariantCage[Cat] //not compiling

    class ContravariantCage[-T]
    val contravariantCage: ContravariantCage[Cat] = new ContravariantCage[Animal]
  }

  def valAndVar(): Unit = {
    class CovariantCage[+T](val animal: T) //covariant position
    class InvariantCage[T](val animal: T)

    //  class ContravariantCage[-T](val animal: T) //not compiling
    //  val cage: ContravariantCage[Cat] = new ContravariantCage[Animal](new Crocodile)


    //  class CovariantVariableCage[+T](var animal: T) //not compiling - contravariant position
    //  val cage: CovariantVariableCage[Animal] = new CovariantVariableCage[Cat](new Cat)
    //  cage.animal = new Crocodile

//      class ContravariantVariableCage[-T](var animal: T) //not compiling - also in covariant position
    //  val cage: ContravariantVariableCage[Cat] = new ContravariantVariableCage[Animal](new Crocodile)

    class InvariantVariableCage[T](var animal: T) //the only ok solution for var
  }

  def collections(): Unit = {
//    class CollectionCovariantCage[+T] {
//      def add(animal: T) = true //not compiling - contravariant position
//    }
//    val cage: CollectionCovariantCage[Animal] = new CollectionCovariantCage[Dog]
//    cage.add(new Cat)

    class CollectionContravariantCage[-T] {
      def add(animal: T) = true
      val cage: CollectionContravariantCage[Cat] = new CollectionContravariantCage[Animal]
      cage.add(new Cat)
      cage.add(new Kitty)
    }
  }

  def wideningAndMethodArguments(): Unit = {
    //method arguments are in contravariant position

    class MyList[+A] {
      def add[B >: A](element: B): MyList[B] = new MyList[B] //widening the type
    }
    val emptyList = new MyList[Kitty]
    val animals = emptyList.add(new Kitty)
    val moreAnimals = emptyList.add(new Cat).add(new Crocodile)
  }

  def returnTypes(): Unit = {
//    class PetShop[-T] {
//      def get(isAPuppy: Boolean): T //not compiling - return types are in covariant position
//    }
//    val catShop = new PetShop[Animal] {
//      def get(isAPuppy: Boolean): Animal = new Cat
//    }
//    val dogShop: PetShop[Dog] = catShop
//    dogShop.get(true) //cat

    class PetShop[-A] {
      def get[B <: A](isAPuppy: Boolean, default: B): B = default
    }

    val shop: PetShop[Dog] = new PetShop[Animal]
//    val evilCat = shop.get(true, new Cat) //not compiling
    class TerraNova extends Dog
    val bigFurry = shop.get(false, new TerraNova)
  }

  def scalaFunctions(): Unit = {
    trait MyFunction[-T1, -T2, -T3, +R] {
      def apply(t1: T1, t2: T2, t3: T3): R
    }
  }

}

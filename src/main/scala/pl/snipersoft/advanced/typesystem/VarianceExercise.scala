package pl.snipersoft.advanced.typesystem

object VarianceExercise extends App {
  class Vehicle
  class Car extends Vehicle
  class Bike extends Vehicle
  class IList[T]

  /*
  use covariant if it's collection of things
  use contravariant if it's group of actions
  here contravariant - we have actions (park, impound, check)
   */

  def withStandardList(): Unit = {
    class IParking[T](things: List[T]) {
      def park(vehicle: T) : IParking[T] = ???
      def impound(vehicles: List[T]): IParking[T] = ???
      def checkVehicles(condition: String): List[T] = ???
      def flatMap[S](f: T => IParking[S]): IParking[S] = ???
    }

    class CParking[+T](things: List[T]) {
      def park[S >: T](vehicle: S) : CParking[S] = ???
      def impound[S >: T](vehicles: List[S]) : CParking[S] = ???
      def checkVehicles(condition: String): List[T] = ???
      def flatMap[S](f: T => CParking[S]): CParking[S] = ???
    }

    class XParking[-T](things: List[T]) {
      def park(vehicle: T): XParking[T] = ???
      def impound(vehicles: List[T]): XParking[T] = ???
      def checkVehicles[S <: T](condition: String): List[S] = ???
//      def flatMap[S](f: Function1[T, XParking[S]]): XParking[S] = ??? //T in function1 is contraviant
      def flatMap[R <: T, S](f: R => XParking[S]): XParking[S] = ???
    }
  }

  def withInvariantList(): Unit = {
    class IParking[T](things: IList[T]) {
      def park(vehicle: T) : IParking[T] = ???
      def impound(vehicles: IList[T]): IParking[T] = ???
      def checkVehicles(condition: String): IList[T] = ???
    }

    class CParking[+T](things: IList[T]) {
      def park[S >: T](vehicle: S) : CParking[S] = ???
      def impound[S >: T](vehicles: IList[S]) : CParking[S] = ???
      def checkVehicles[S >: T](condition: String): IList[S] = ???
    }

    class XParking[-T](things: IList[T]) {
      def park(vehicle: T): XParking[T] = ???
      def impound[S <: T](vehicles: IList[S]): XParking[S] = ???
      def checkVehicles[S <: T](condition: String): IList[S] = ???
    }
  }


}

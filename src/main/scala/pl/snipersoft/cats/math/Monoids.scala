package pl.snipersoft.cats.math

import cats.Semigroup
import cats.instances.int._
import cats.Monoid
import cats.instances.int._ //Monoid[Int]
import cats.instances.option._ //Monoid[Option[Int]]
import cats.implicits._

//semigroup with initial value
object Monoids extends App {
  semigroupStartValueProblem()
  monoids()
  extensionMethod()
  reducePhonebookExercise()
  shoppingCardExercise()

  def semigroupStartValueProblem(): Unit = {
    import cats.syntax.semigroup._
    val numbers = (1 to 1000).toList
    //|+| is always associative
    val sumLeft = numbers.foldLeft(0)(_ |+| _)
    val sumRight = numbers.foldRight(0)(_ |+| _)
    assert(sumLeft == sumRight)

//    def combineFold[T : Semigroup](list: List[T]): T = list.foldLeft(/* WHAT SHOULD BE START VALUE? */)(_ |+| _)
  }

  def monoids(): Unit = {
    Monoid[Int].combine(2, 3) //5
    Monoid[Int].empty //0
    Monoid[Int].isEmpty(0)
    Monoid[Option[Int]].empty //None
    val monoid: Semigroup[Int] = Monoid[Int] //Monoid extends Semigroup
    Monoid[Option[Int]].combine(2.some, Option.empty) //Some(2)
  }

  def extensionMethod(): Unit = {
    2.some |+| Option.empty
  }

  def reducePhonebookExercise(): Unit = {
    def reduce[T](list: List[T])(implicit monoid: Monoid[T]): T =
      list.foldLeft(monoid.empty)(_ |+| _)

    import cats.instances.map._
    val phonebook1 = Map("Janek" -> "656895475", "Tomek" -> "785544223")
    val phonebook2 = Map("Janek" -> "655983215", "Marysia" -> "788542156")

    println(reduce(List(phonebook1, phonebook2))) //Janek -> 656895475655983215!
  }

  def shoppingCardExercise(): Unit = {
    case class ShoppingCard(items: List[String], total: Double)
    object ShoppingCard {
      private val empty = new ShoppingCard(List(), 0)

      private val shoppingCardMonoid: Monoid[ShoppingCard] =
        Monoid.instance(empty, (c1, c2) => ShoppingCard(c1.items ++ c2.items, c1.total + c2.total))

      def checkout(cards: List[ShoppingCard]): ShoppingCard = shoppingCardMonoid.combineAll(cards)
    }

    val c1 = ShoppingCard(List("Kar98k", "G-43"), 3000)
    val c2 = ShoppingCard(List("Garand", "Thomson"), 200)
    val c3 = ShoppingCard(List("Ppsh 41"), 10)

    println(ShoppingCard.checkout(List(c1, c2, c3)))
  }
}

package pl.snipersoft.cats.math

import cats.Semigroup
import cats.implicits._

//combines elements of the same type
object Semigroups extends App {
  combination()
  reduction()
  customType()
  extensionMethods()
  reductionWithExtensionMethods()

  def combination(): Unit = {
    val intCombination = Semigroup[Int].combine(2, 46) //2+46
    val stringCombination = Semigroup[String].combine("a", "bc") //abc
    println(intCombination)
    println(stringCombination)
  }

  def reduction(): Unit = {
    def reduce[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

    val numbers = (1 to 10).toList
    val numberOptions = (1 to 10).map(_.some).toList
    val secondNumberOptions = None :: numberOptions
    println(reduce(numbers)) //55
    println(reduce(numberOptions)) //Some(55)
    println(reduce(secondNumberOptions)) //Some(55)
//    println(reduce[Int](List())) //UnsupportedOperationException!
  }

  def customType(): Unit = {
    case class Expense(id: Long, amount: Double)
    implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (o1, o2) =>
      Expense(Math.max(o1.id, o2.id), o1.amount + o2.amount)
    }

    val combined = Semigroup[Expense].combine(Expense(2, 100.25), Expense(5, 1000.11))
    println(combined)
  }

  def extensionMethods(): Unit = {
    import cats.implicits._
    println("2".some |+| "3".some)
    println(2 |+| 3)
  }

  def reductionWithExtensionMethods(): Unit = {
    //add implicit parameter Semigroup[T]
    def reduce[T : Semigroup](list: List[T]): T = list.reduce(_ |+| _)

    val numbers = (1 to 10).toList
    println(reduce(numbers)) //55
  }
}

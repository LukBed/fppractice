package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FlatMapSpec extends AnyFunSuite with Matchers {
  test("should flat map the list") {
    val list = 1 :: 3 :: 5 :: MyNil
    val expected = 1 :: 11 :: 111 :: 3 :: 33 :: 333 :: 5 :: 55 :: 555 :: MyNil
    val function = (i: Int) => i :: i*11 :: i*111 :: MyNil
    list.flatMap(function) shouldBe expected
  }

  test("should flat map the big list") {
    val list = MyList.from(1 to 1000000)
    val function = (i: Int) => i :: i*11 :: i*111 :: MyNil
    list.flatMap(function).length shouldBe 3000000
  }

  test("should flat map the Scala list (for time comparison)") {
    val list = (1 to 1000000).toList
    val function = (i: Int) => List(i, i*11, i*111)
    list.flatMap(function).length shouldBe 3000000
  }

  val rockList = MyList.from(1 to 10000)
  test("should flat map the rock the JVM list") {
    rockList.flatMap(x => x :: x*2 :: MyNil)
  }
}
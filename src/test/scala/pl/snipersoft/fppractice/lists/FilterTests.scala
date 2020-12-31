package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FilterTests extends AnyFunSuite with Matchers {
  test("should filter records from list") {
    val list = MyList.from(0 to 21)
    val expected = 0 :: 3 :: 6 :: 9 :: 12 :: 15 ::18 :: 21 :: MyNil
    val function = (i: Int) => i%3 == 0
    list.filter(function) shouldBe expected
  }

  test("should filter record from big list") {
    val list = MyList.from(1 to 1000000)
    list.filter(_%2 == 0).length shouldBe 500000
  }
}
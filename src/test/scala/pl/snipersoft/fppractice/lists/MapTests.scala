package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MapTests extends AnyFunSuite with Matchers {
  test("should map the list") {
    val list = 1 :: 3 :: 5 :: MyNil
    val expected = 2 :: 6 :: 10 :: MyNil
    list.map(_*2) shouldBe expected
  }

  test("should map the big list") {
    val list = MyList.from(1 to 1000000)
    list.map(_+1).length shouldBe 1000000
  }
}
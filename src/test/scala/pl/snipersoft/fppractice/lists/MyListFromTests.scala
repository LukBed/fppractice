package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MyListFromTests extends AnyFunSuite with Matchers {
  test("should create list from range") {
    MyList.from(1 to 5) shouldBe (1 :: 2 :: 3 :: 4 :: 5 :: MyNil)
  }

  test("should create list from big range") {
    val list = MyList.from(1 to 1000000)
    list(0) shouldBe 1
    list(999999) shouldBe 1000000
  }
}

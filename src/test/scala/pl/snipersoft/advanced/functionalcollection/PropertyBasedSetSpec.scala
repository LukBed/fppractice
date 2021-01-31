package pl.snipersoft.advanced.functionalcollection

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PropertyBasedSetSpec extends AnyFunSuite with Matchers {
  val abcSet: MySet[String] = MySet("a", "b", "c")
  val noAbcSet = !abcSet

  test("should create negation of set") {
    noAbcSet("a") shouldBe false
    noAbcSet("b") shouldBe false
    noAbcSet("c") shouldBe false
    noAbcSet("d") shouldBe true
    noAbcSet("e") shouldBe true
  }

  test("should add element to set") {
    val newSet = noAbcSet + "c"

    newSet("a") shouldBe false
    newSet("b") shouldBe false
    newSet("c") shouldBe true
    newSet("d") shouldBe true
    newSet("e") shouldBe true
  }

  test("should add another set") {
    val newSet = abcSet ++ MySet("c", "d", "e")

    newSet("a") shouldBe true
    newSet("b") shouldBe true
    newSet("c") shouldBe true
    newSet("d") shouldBe true
    newSet("e") shouldBe true
    newSet("f") shouldBe false
    newSet("g") shouldBe false
  }

  test("should remove element from set") {
    val newSet = abcSet - "c"

    newSet("a") shouldBe true
    newSet("b") shouldBe true
    newSet("c") shouldBe false
    newSet("d") shouldBe false
    newSet("e") shouldBe false
  }

  test("should remove another set from set") {
    val newSet = abcSet -- MySet("b", "c")

    newSet("a") shouldBe true
    newSet("b") shouldBe false
    newSet("c") shouldBe false
    newSet("d") shouldBe false
    newSet("e") shouldBe false
  }

  test("should create intersections of two sets") {
    val intersection = abcSet & MySet("a", "c")

    intersection("a") shouldBe true
    intersection("b") shouldBe false
    intersection("c") shouldBe true
    intersection("d") shouldBe false
    intersection("e") shouldBe false
  }
}

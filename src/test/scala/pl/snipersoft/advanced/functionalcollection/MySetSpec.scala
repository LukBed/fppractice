package pl.snipersoft.advanced.functionalcollection

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MySetSpec extends AnyFunSuite with Matchers {
  val abcSet: MySet[String] = MySet("a", "b", "c")
  val fiveSet: MySet[Int] = MySet(1,2,3,4,5)

  test("should check if set contains element") {
    abcSet contains "a" shouldBe true
    abcSet("a") shouldBe true
    abcSet contains "b" shouldBe true
    abcSet("b") shouldBe true
    abcSet contains "c" shouldBe true
    abcSet("c") shouldBe true
    abcSet contains "d" shouldBe false
    abcSet("d") shouldBe false

    MySet() contains "a" shouldBe false
    MySet()("a") shouldBe false
  }

  test("should add element to set") {
    val addedSet = abcSet + "d"
    addedSet("a") shouldBe true
    addedSet("b") shouldBe true
    addedSet("c") shouldBe true
    addedSet("d") shouldBe true
    addedSet("e") shouldBe false

    abcSet + "a" shouldBe abcSet

    val oneElementSet = MySet() + "a"
    oneElementSet("a") shouldBe true
    oneElementSet("b") shouldBe false
  }

  test("should add set to set") {
    val setToBeAdded = MySet("c", "d", "e")
    MySet() ++ setToBeAdded shouldBe setToBeAdded

    val joinedSets = abcSet ++ setToBeAdded
    joinedSets("a") shouldBe true
    joinedSets("b") shouldBe true
    joinedSets("c") shouldBe true
    joinedSets("d") shouldBe true
    joinedSets("e") shouldBe true
    joinedSets("f") shouldBe false
  }

  test("should remove element") {
    val abSet = abcSet - "b"
    abSet("a") shouldBe true
    abSet("c") shouldBe true
    abSet("b") shouldBe false

    MySet() - "b" shouldBe MySet()

    abcSet - "d" shouldBe abcSet
  }

  test("should intersect two sets") {
    val firstSet = MySet("a", "b", "c", "d")
    val secondSet = MySet("a", "c", "e", "f")
    val intersection = firstSet & secondSet

    intersection("a") shouldBe true
    intersection("c") shouldBe true
    intersection("b") shouldBe false
    intersection("d") shouldBe false
    intersection("e") shouldBe false
    intersection("f") shouldBe false
  }

  test("should find differences between two sets") {
    val firstSet = MySet("a", "b", "c", "d")
    val secondSet = MySet("a", "c", "e", "f")
    val differences = firstSet -- secondSet

    differences("b") shouldBe true
    differences("d") shouldBe true
    differences("a") shouldBe false
    differences("c") shouldBe false
    differences("e") shouldBe false
    differences("f") shouldBe false
    differences("g") shouldBe false
  }

  test("should map set") {
    val mapFunction: String => String = (text: String) => text + "a"
    val mappedSet = abcSet map mapFunction

    mappedSet("a") shouldBe false
    mappedSet("b") shouldBe false
    mappedSet("c") shouldBe false
    mappedSet("aa") shouldBe true
    mappedSet("ba") shouldBe true
    mappedSet("ca") shouldBe true

    MySet() map mapFunction shouldBe MySet()
  }

  test("should flat map set") {
    val mapFunction: String => MySet[String] = (text: String) => MySet(text, text + "a")
    val mappedSet = abcSet flatMap mapFunction

    mappedSet("a") shouldBe true
    mappedSet("b") shouldBe true
    mappedSet("c") shouldBe true
    mappedSet("d") shouldBe false
    mappedSet("aa") shouldBe true
    mappedSet("ba") shouldBe true
    mappedSet("ca") shouldBe true
    mappedSet("da") shouldBe false

    MySet() flatMap mapFunction shouldBe MySet()
  }

  test("should filter set") {
    val filterFunction = (n: Int) => n%2 == 1
    val filteredSet = fiveSet filter filterFunction
    filteredSet(1) shouldBe true
    filteredSet(3) shouldBe true
    filteredSet(5) shouldBe true
    filteredSet(2) shouldBe false
    filteredSet(4) shouldBe false

    MySet() filter filterFunction shouldBe MySet()
  }

  test("should execute action foreach") {
    var text = ""
    val action = (s: String) => text +=  s
    abcSet foreach action

    text.length shouldBe 3
    text contains "a" shouldBe true
    text contains "b" shouldBe true
    text contains "c" shouldBe true
  }
}

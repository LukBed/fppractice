package pl.snipersoft.advanced.functionalcollection

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MySetSpec extends AnyFunSuite with Matchers {
  val abcSet: MySet[String] = NonEmptySet("a", NonEmptySet("b", NonEmptySet("c", EmptySet())))
  val fiveSet: MySet[Int] = NonEmptySet(1, NonEmptySet(2, NonEmptySet(3, NonEmptySet(4, NonEmptySet(5, EmptySet())))))

  test("should check if set contains element") {
    abcSet.contains("a") shouldBe true
    abcSet("a") shouldBe true
    abcSet.contains("b") shouldBe true
    abcSet("b") shouldBe true
    abcSet.contains("c") shouldBe true
    abcSet("c") shouldBe true
    abcSet.contains("d") shouldBe false
    abcSet("d") shouldBe false

    EmptySet().contains("a") shouldBe false
    EmptySet()("a") shouldBe false
  }

  test("should add element to set") {
    val addedSet = abcSet + "d"
    addedSet("a") shouldBe true
    addedSet("b") shouldBe true
    addedSet("c") shouldBe true
    addedSet("d") shouldBe true
    addedSet("e") shouldBe false

    abcSet + "a" shouldBe abcSet

    val oneElementSet = EmptySet() + "a"
    oneElementSet("a") shouldBe true
    oneElementSet("b") shouldBe false
  }

  test("should add set to set") {
    val setToBeAdded = NonEmptySet("c", NonEmptySet("d", NonEmptySet("e", EmptySet())))
    EmptySet() ++ setToBeAdded shouldBe setToBeAdded

    val joinedSets = abcSet ++ setToBeAdded
    joinedSets("a") shouldBe true
    joinedSets("b") shouldBe true
    joinedSets("c") shouldBe true
    joinedSets("d") shouldBe true
    joinedSets("e") shouldBe true
    joinedSets("f") shouldBe false
  }

  test("should map set") {
    val mapFunction: String => String = (text: String) => text + "a"
    val mappedSet = abcSet.map(mapFunction)

    mappedSet("a") shouldBe false
    mappedSet("b") shouldBe false
    mappedSet("c") shouldBe false
    mappedSet("aa") shouldBe true
    mappedSet("ba") shouldBe true
    mappedSet("ca") shouldBe true

    EmptySet().map(mapFunction) shouldBe EmptySet()
  }

  test("should flat map set") {
    val mapFunction: String => MySet[String] = (text: String) => NonEmptySet(text, NonEmptySet(text + "a", EmptySet()))
    val mappedSet = abcSet.flatMap(mapFunction)

    mappedSet("a") shouldBe true
    mappedSet("b") shouldBe true
    mappedSet("c") shouldBe true
    mappedSet("d") shouldBe false
    mappedSet("aa") shouldBe true
    mappedSet("ba") shouldBe true
    mappedSet("ca") shouldBe true
    mappedSet("da") shouldBe false

    EmptySet().flatMap(mapFunction) shouldBe EmptySet()
  }

  test("should filter set") {
    val filterFunction = (n: Int) => n%2 == 1
    val filteredSet = fiveSet.filter(filterFunction)
    filteredSet(1) shouldBe true
    filteredSet(3) shouldBe true
    filteredSet(5) shouldBe true
    filteredSet(2) shouldBe false
    filteredSet(4) shouldBe false

    EmptySet().filter(filterFunction) shouldBe EmptySet()
  }

  test("should execute action foreach") {
    var text = ""
    val action = (s: String) => text +=  s
    abcSet.foreach(action)

    text shouldBe "abc"
  }
}

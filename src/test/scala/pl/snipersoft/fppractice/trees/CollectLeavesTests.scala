package pl.snipersoft.fppractice.trees

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CollectLeavesTests extends AnyFunSuite with Matchers {
  test("empty tree should not have leaves") {
    BEnd.collectLeaves shouldBe List.empty
    BEnd.leafCount shouldBe 0
  }

  test("should find leaves") {
    val h = BNode("h")
    val g = BNode("g", h)
    val f = BNode("f")
    val e = BNode("e")
    val d = BNode("d")
    val c = BNode("c", e, f)
    val b = BNode("b", c, d)
    val a = BNode("a", b, g)
    val expectedLeaves = List(e, f, d, h)

    a.collectLeaves should contain theSameElementsAs expectedLeaves
    a.leafCount shouldBe 4
  }

}

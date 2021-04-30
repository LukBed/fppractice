package pl.snipersoft.fppractice.trees

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class IsLeafSpec extends AnyFunSuite with Matchers {
  test("empty tree should not be leaf") {
    BEnd.isLeaf shouldBe false
  }

  test("tree with left node should not be leaf") {
    val tree = BNode(value = "a", left = BNode("b"), right = BEnd)
    tree.isLeaf shouldBe false
  }

  test("tree with right node should not be leaf") {
    val tree = BNode(value = "a", left = BEnd, right = BNode("b"))
    tree.isLeaf shouldBe false
  }

  test("tree with two nodes should not be leaf") {
    val tree = BNode(value = "a", left = BNode("b"), right = BNode("c"))
    tree.isLeaf shouldBe false
  }

  test("tree without nodes should be leaf") {
    val tree = BNode(value = "a", left = BEnd, right = BEnd)
    tree.isLeaf shouldBe true
  }
}

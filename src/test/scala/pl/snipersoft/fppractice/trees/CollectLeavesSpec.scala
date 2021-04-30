package pl.snipersoft.fppractice.trees

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class CollectLeavesSpec extends AnyFunSuite with Matchers {
  val data = Table(("nr", "tree", "leaves"),
    (1, Trees.a, List(Trees.aaaa, Trees.aaab, Trees.aab, Trees.aba)),
    (2, Trees.aa, List(Trees.aaaa, Trees.aaab, Trees.aab)),
    (3, Trees.ab, List(Trees.aba)))

  test("empty tree should not have leaves") {
    BEnd.collectLeaves shouldBe List.empty
    BEnd.leafCount shouldBe 0
  }

  forAll(data) { (nr: Int, tree: BTree[String], leaves: List[BTree[String]]) =>
    test(s"should find leaves - case $nr") {
      tree.collectLeaves should contain theSameElementsAs leaves
      tree.leafCount shouldBe leaves.size
    }
  }
}

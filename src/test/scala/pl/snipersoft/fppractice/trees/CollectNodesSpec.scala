package pl.snipersoft.fppractice.trees

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class CollectNodesSpec extends AnyFunSuite with Matchers {
  val data = Table(("tree", "level", "nodes"),
    (Trees.a, 0, List(Trees.a)),
    (Trees.a, 1, List(Trees.aa, Trees.ab)),
    (Trees.a, 2, List(Trees.aaa, Trees.aab, Trees.aba)),
    (Trees.a, 3, List(Trees.aaaa, Trees.aaab)),
    (Trees.a, 4, Nil))

  forAll(data) { (tree: BTree[String], level: Int, nodes: List[BTree[String]]) =>
    test(s"should collect nodes of tree - level $level") {
      tree.collectNodes(level) should contain theSameElementsAs nodes
    }
  }

  test("should not collect nodes for negative level") {
    Trees.a.collectNodes(-1) shouldBe Nil
    Trees.a.collectNodes(-10) shouldBe Nil
  }
}

package pl.snipersoft.fppractice.trees

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class SizeSpec extends AnyFunSuite with Matchers {
  val data = Table(("nr", "tree", "size"),
    (1, Trees.a, 7),
    (2, Trees.aa, 4),
    (3, Trees.aab, 0),
    (4, Trees.aaa, 2),
    (5, Trees.ab, 1))

  forAll(data) { (nr: Int, tree: BTree[String], size: Int) =>
    test(s"should calculate size for tree - case $nr") {
      tree.size shouldBe size
    }
  }

  test("should calculate size for big tree") {
    val tree = (0 to 100000).foldLeft[BTree[Int]](BEnd)((tree, number) => BNode(number, tree, BEnd))
    tree.size shouldBe 100000
  }
}

package pl.snipersoft.fppractice.trees

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MirrorTests extends AnyFunSuite with Matchers {
  test("should mirror the tree") {
    /*
       _____a_____
      /           \
     _ab_        __aa_
    /    \      /     \
    end   aba   aab    _aaa_
                  /     \
                  aaab   aaaa
  */

    val aba = BNode("aba")
    val ab = BNode("ab", BEnd, aba)
    val aaab = BNode("aaab")
    val aaaa = BNode("aaaa")
    val aab = BNode("aab")
    val aaa = BNode("aaa", aaab, aaaa)
    val aa = BNode("aa", aab, aaa)
    val aMirror: BTree[String] = BNode("a", ab, aa)

    Trees.a.mirror shouldBe aMirror
  }
}

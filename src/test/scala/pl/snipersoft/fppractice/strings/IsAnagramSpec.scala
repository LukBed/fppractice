package pl.snipersoft.fppractice.strings

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import org.scalatest.prop.TableDrivenPropertyChecks._

import StringUtils._

class IsAnagramSpec extends AnyFunSuite with Matchers {
  val anagrams = Table(("s1", "s2"), ("Abcd", "dcbA"), ("MyBooK", "KooByM"), ("a", "a"))
  val noAnagrams = Table(("s1", "s2"), ("AbcD", "dcbA"), ("Scala", "Java"), ("a", "A"), ("Abcde", "edCbA"))

  forAll(anagrams) { (s1: String, s2: String) =>
    test(s"$s1 should be anagram of $s2") {
      s1.isAnagramOf(s2) shouldBe true
    }
  }

  forAll(noAnagrams) { (s1: String, s2: String) =>
    test(s"$s1 should not be anagram of $s2") {
      s1.isAnagramOf(s2) shouldBe false
    }
  }

  test("empty string should be anagram of empty string") {
    "".isAnagramOf("") shouldBe true
  }
}

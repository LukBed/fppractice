package pl.snipersoft.fppractice.strings

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import org.scalatest.prop.TableDrivenPropertyChecks._
import StringUtils._

class CountCharactersTests extends AnyFunSuite with Matchers {
  val data = Table(
    ("input", "expected"),
    ("Scala", Map('S' -> 1, 'c' -> 1, 'a' -> 2, 'l' -> 1)),
    ("AbabaKa", Map('A' -> 1, 'b' -> 2, 'a' -> 3, 'K' -> 1)))

  forAll(data) { (input: String, expected: Map[Char, Int]) =>
    test (s"should count characters for text $input") {
      input.countCharacters shouldBe expected
    }
  }

  test("should return empty map for empty string") {
    "".countCharacters shouldBe Map.empty
  }
}

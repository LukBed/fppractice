package pl.snipersoft.fppractice.strings

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import org.scalatest.prop.TableDrivenPropertyChecks._
import StringUtils._

class HasValidParenthesesTests extends AnyFunSuite with Matchers {
  val validParentheses = Table("input", "()", "()()", "(())")
  val invalidParentheses = Table("input", ")(", "))((", ")(()", "()()(", "())(")
  val invalidFormat = Table("input", "a", "(a)")

  forAll(validParentheses) { s: String =>
    test(s"$s should be evaluated as with valid parentheses") {
      s.hasValidParentheses shouldBe true
    }
  }

  forAll(invalidParentheses) { s: String =>
    test(s"$s should be evaluated as with invalid parentheses") {
      s.hasValidParentheses shouldBe false
    }
  }

  forAll(invalidFormat) { s: String =>
    test(s"$s with invalid format should be evaluated as with invalid parentheses") {
      s.hasValidParentheses shouldBe false
    }
  }

  test("empty string should be evaluated as with valid parentheses") {
    "".hasValidParentheses shouldBe true
  }
}

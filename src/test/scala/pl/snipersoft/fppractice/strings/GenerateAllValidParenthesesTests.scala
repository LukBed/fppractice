package pl.snipersoft.fppractice.strings

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import org.scalatest.prop.TableDrivenPropertyChecks._

class GenerateAllValidParenthesesTests extends AnyFunSuite with Matchers {
  val data = Table(("length", "expectedParentheses"),
    (0, List()),
    (1, List("()")),
    (2, List("()()", "(())")),
    (3, List("()()()", "()(())", "(())()", "((()))", "(()())"))
  )

  forAll(data) { (length: Int, expectedParentheses: List[String]) =>
    test(s"should generate all correct parentheses for n=$length") {
      StringUtils.generateAllValidParentheses(length) should contain theSameElementsAs expectedParentheses
    }
  }
}

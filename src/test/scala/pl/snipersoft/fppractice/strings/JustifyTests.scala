package pl.snipersoft.fppractice.strings

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import StringUtils._

class JustifyTests extends AnyFunSuite with Matchers {
  test("should justify text") {
    val input = "Abc def gh ijklmn op rstuw xyza. Bcd efghij kl mn opr stu."
    val expected =
      """Abc    def   gh
        |ijklmn op rstuw
        |xyza.       Bcd
        |efghij   kl  mn
        |opr stu.""".stripMargin

    val result = input.justify(15)
    println(result)

    result shouldBe expected
  }

  test("should correct justify last line with a single words") {
    val input = "Abc def gh ijklmn op rstuw xyza. Bcd efghij kl mn opr."
    val expected =
      """Abc    def   gh
        |ijklmn op rstuw
        |xyza.       Bcd
        |efghij   kl  mn
        |opr.""".stripMargin

    val result = input.justify(15)

    result shouldBe expected
  }

  test("should not justify text with single line") {
    val input = "Ab cde fghi jklm nopr."

    input.justify(30) shouldBe input
  }

  test("should correct justify text with line with single word inside") {
    val input = "Abcde."

    input.justify(30) shouldBe input
  }

  test("should not justify text longer with one word longer than formatted length") {
    val input = "Abcdefghijklmnopr"

    input.justify(3) shouldBe input
  }

  test("should not justify text as long as formatted length") {
    val input = "Abcde."

    input.justify(6) shouldBe input
  }
}

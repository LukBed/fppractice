package pl.snipersoft.fppractice.strings

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import StringUtils._

class JustifyTests extends AnyFunSuite with Matchers {
  test("should justify text") {
    val input = "Abc def gh ijklmn op rstuw xyza. Bcd efghij kl mn opr."
    val expected =
      """Abc    def   gh
        |ijklmn op rstuw
        |xyza.       Bcd
        |efghij   kl  mn
        |opr.""".stripMargin

    input.justify(15) shouldBe expected
  }

}

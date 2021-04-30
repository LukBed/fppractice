package pl.snipersoft.fppractice.strings

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import pl.snipersoft.fppractice.strings.StringUtils.RichString

class ReverseWordsSpec extends AnyFunSuite with Matchers {

  val data = Table(
    ("input", "output"),
    ("Alice loves Scala", "Scala loves Alice"),
    ("  hello    world  ", "world hello"),
    ("hello    world  ", "world hello"),
    ("hello    world", "world hello"),
    ("", ""),
    ("   ", ""))

  forAll(data) { (input: String, output: String) =>
    test(s"should reverse words: '$input'") {
      input.reverseWords shouldBe output
    }
  }

}

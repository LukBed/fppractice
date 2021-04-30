package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RunLengthEncodingSpec extends AnyFunSuite with Matchers {
  test("should calculate RLE (Run Length Encoding) for list") {
    val list = "a" :: "a" :: "b" :: "c" :: "a" :: "c" :: "d" :: MyNil
    val expected: MyList[(String, Int)] = ("a", 3) :: ("b", 1) :: ("c", 2) :: ("d", 1) :: MyNil
    list.rle() shouldBe expected
  }
}
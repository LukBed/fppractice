package pl.snipersoft.fppractice.lists

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SampleSpec extends AnyFunSuite with Matchers {
  val list = "a" :: "b" :: "c" :: MyNil

  test("should generate correct sample") {
    val sample = list.sample(100)
    println(sample)
    sample.length shouldBe 100
    sample.filter(s => s == "a" || s == "b" || s == "c").length shouldBe 100
  }

  test("should return empty list for negative numbers of sample") {
    list.sample(-1) shouldBe MyNil
  }

  test("should return empty list for zero of sample") {
    list.sample(0) shouldBe MyNil
  }
}

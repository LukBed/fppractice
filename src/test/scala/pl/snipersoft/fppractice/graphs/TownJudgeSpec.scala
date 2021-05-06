package pl.snipersoft.fppractice.graphs

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TownJudgeSpec extends AnyFunSuite with Matchers {
  test("should find judge") {
    val n = 3
    val trust = List(
      (1, 2),
      (3, 2),
      (1, 3))

    TownJudge.findJudge(n, trust) shouldBe Some(2)
  }

  test("should not find judge if there is not a person trusted by everyone") {
    val n = 3
    val trust = List(
      (1, 2),
      (3, 1),
      (1, 3))

    TownJudge.findJudge(n, trust) shouldBe None
  }

  test("should not find judge if there is possible judge trusts anyone") {
    val n = 3
    val trust = List(
      (1, 2),
      (3, 2),
      (1, 3),
      (2, 1))

    TownJudge.findJudge(n, trust) shouldBe None
  }
}

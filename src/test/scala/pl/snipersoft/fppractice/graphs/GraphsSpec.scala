package pl.snipersoft.fppractice.graphs

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import pl.snipersoft.fppractice.graphs.Graphs.Graph

class GraphsSpec extends AnyFunSuite with Matchers {
  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Bob", "Mary"),
    "Mary" -> Set("Bob", "Charlie"))


  val outDegreesData = Table(("person", "outDegree"),
    ("Alice", 3), ("Bob", 0), ("Charlie", 1), ("David", 2), ("Mary", 2), ("Marley", 0))

  forAll(outDegreesData) { (person: String, outDegree: Int) =>
    test(s"should calculate out degree in social network for $person as $outDegree") {
      Graphs.outDegree(socialNetwork, person) shouldBe outDegree
    }
  }

  val inDegreesData = Table(("person", "inDegree"),
    ("Alice", 0), ("Bob", 3), ("Charlie", 2), ("David", 2), ("Mary", 1), ("Marley", 0))

  forAll(inDegreesData) { (person: String, inDegree: Int) =>
    test(s"should calculate in degree in social network for $person as $inDegree") {
      Graphs.inDegree(socialNetwork, person) shouldBe inDegree
    }
  }

  val isPathData = Table(("from", "to", "isPath"),
    ("Alice", "Mary", true), ("Mary", "Alice", false), ("Bob", "Charlie", false), ("Alice", "Bob", true), ("Charlie", "David", true))

  forAll(isPathData) { (from: String, to: String, isPath: Boolean) =>
    test(s"should check is path from $from to $to as $isPath") {
      Graphs.isPath(socialNetwork, from, to) shouldBe isPath
    }
  }
}

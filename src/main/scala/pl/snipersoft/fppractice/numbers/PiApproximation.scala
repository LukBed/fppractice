package pl.snipersoft.fppractice.numbers

import scala.annotation.tailrec
import scala.util.Random

//Monte-Carlo algorithm: generate points with coordinates from (0, 0) to (1, 0)
//and check how much points are inside circle with radius 1
//points inside / total points = area of circle / area of square = Pi / 4
object PiApproximation {
  val randomGenerator = new Random()

  def apply(points: Int): Double = {
    @tailrec
    def helper(pointsToGenerate: Int, pointsInside: Int, generatedPoints: Int): Double = {
      if (pointsToGenerate <= 0) 4.0 * pointsInside / generatedPoints
      else if (generatePoint().isInCircle) helper(pointsToGenerate - 1, pointsInside + 1, generatedPoints + 1)
      else helper(pointsToGenerate - 1, pointsInside, generatedPoints + 1)
    }

    helper(points, 0, 0)
  }

  private def generatePoint(): Point = Point(randomGenerator.nextDouble(), randomGenerator.nextDouble())

  private case class Point(x: Double, y: Double) {
    def isInCircle: Boolean = x * x + y * y <= 1
  }

}

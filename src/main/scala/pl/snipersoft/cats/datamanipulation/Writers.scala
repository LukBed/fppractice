package pl.snipersoft.cats.datamanipulation

import cats.data.Writer

import scala.annotation.tailrec

//logs type and value type
object Writers extends App {

  introduction()
  composition()
  countAndSayExercise()
  sumExercise()

  def introduction(): Unit = {
    val writer: Writer[List[String], Int] = Writer(List("a", "b", "c"), 21)
    val increaseWriter = writer.map(_+1) //value changed, logs stay the same
    val addLogWriter = writer.mapWritten(_ :+ "d") //logs changed, value stays the same
    val bothWriter = writer.bimap(_ :+ "d", _+1) //value and logs changed
    val anotherBothWriter = writer.mapBoth { (logs, value) => (logs :+ "d", value+1) } //value and logs changed
    val emptyLogsWriter = writer.reset //can use monoid

    println(bothWriter.value)
    println(bothWriter.written)
    println(bothWriter.run) //tuple
    println(emptyLogsWriter.run)
  }

  def composition(): Unit = {
    val writer = for {
      a <- Writer(Vector("a", "b"), 10)
      b <- Writer(Vector("c", "d"), 5)
    } yield a+b //can use semigroup to combine values
    println(writer.run)
  }

  def countAndSayExercise(): Unit = {
//    countAndSay(10)
    println(countAndLog(10).run)

    def countAndSay(n: Int): Unit =
      if (n <= 0) println("starting!")
      else { countAndSay(n-1); println(n)}

    def countAndLog(n: Int): Writer[Vector[String], Int] = {
      @tailrec
      def tailRec(acc: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
        if (acc.value <= 0) acc
        else {
          val changed = acc.mapBoth{ (log, value) => (log :+ value.toString, value-1) }
          tailRec(changed)
        }
      }

      tailRec(Writer(Vector(), n))
    }
  }

  def sumExercise(): Unit = {
    println(naiveSum(10))
    println(sumAndLog(10).run)
    println(sumAndLogFor(10).run)

    def naiveSum(n: Int): Int =
      if (n<=0) 0
      else {
        println(s"Now at $n")
        val lowerSum = naiveSum(n-1)
        println(s"Computed sum (${n-1}) = $lowerSum")
        lowerSum + n
      }

    def sumAndLog(n: Int): Writer[Vector[String], Int] = {
      @tailrec
      def tailRec(acc: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
        val x = acc.written.size /2
        if (x>n) acc
        else {
          val newAcc = acc.mapBoth { (logs, value) =>
            val newSum = value + x
            (logs :+ s"Now at $n" :+ s"Computed sum ($x) = $newSum", newSum)
          }
          tailRec(newAcc)
        }
      }

      tailRec(Writer(Vector(), 0))
    }

    def sumAndLogFor(n: Int): Writer[Vector[String], Int] =
      if (n <= 0) Writer(Vector(), 0)
      else for {
        _ <- Writer(Vector(s"Now at $n"), n) //n here is ignored
        lowerSum <- sumAndLog(n-1)
        _ <- Writer(Vector(s"Computed sum ($n-1) = $lowerSum"), n)
      } yield lowerSum + n //all logs will be combined!
  }
}

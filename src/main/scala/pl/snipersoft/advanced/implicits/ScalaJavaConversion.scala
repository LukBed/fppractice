package pl.snipersoft.advanced.implicits

import java.util
import java.util.Optional

object ScalaJavaConversion extends App {

  import java.{util => ju}

  //deprecated
  import collection.JavaConverters._

  toScala()
  toJavaAndBack()
  optionalOption()

  def toScala(): Unit = {
    val javaSet: ju.Set[Int] = new util.HashSet[Int]()
    (1 to 5).foreach(javaSet.add)
    println(javaSet)

    val scalaSet = javaSet.asScala
  }

  def toJavaAndBack(): Unit = {
    import collection.mutable._
    val numbersBuffer = ArrayBuffer[Int](1, 2, 3)
    val juNumbersBuffer = numbersBuffer.asJava
    val numbersBuffer2 = juNumbersBuffer.asScala
    assert(numbersBuffer eq numbersBuffer2) //the same reference

    val numbers = List(1, 2, 3) //immutable list
    val juNumbers = numbers.asJava
    val numbers2 = juNumbers.asScala
    assert(!(numbers eq numbers2)) //not the same reference (different types)
    assert(numbers == numbers2) //the same elements

//    juNumbers.add(7) //immutable so UnsupportedOperationException
  }

  def optionalOption(): Unit = {

    class ToScala[S](v: => S) {
      def asScala: S = v
    }

    implicit def optionalAsScala[T](o: Optional[T]) = new ToScala[Option[T]](if (o.isPresent) Some(o.get()) else None)

    val emptyOptional: Optional[String] = Optional.empty()
    println(emptyOptional.asScala)
    println(Optional.ofNullable("abc").asScala)
  }

}

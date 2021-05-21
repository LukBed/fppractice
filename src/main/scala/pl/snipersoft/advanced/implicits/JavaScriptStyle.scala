package pl.snipersoft.advanced.implicits

object JavaScriptStyle extends App {
  implicit def stringToInt(s: String): Int = Integer.valueOf(s)

  println("6" / 2) //stringToInt("6")/2
}

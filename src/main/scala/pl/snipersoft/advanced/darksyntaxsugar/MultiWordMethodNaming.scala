package pl.snipersoft.advanced.darksyntaxsugar

object MultiWordMethodNaming extends App {
  case class TeenGirl(name: String) {
    def `and then said`(gossip: String) = println(s"$name said $gossip")
  }

  val marysia = TeenGirl("Marysia")
  marysia `and then said` "I like Scala"
}
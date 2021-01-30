package pl.snipersoft.advanced.darksyntaxsugar

object Setters extends App {
  class Mutable {
    private var internalMember: Int = 0
    def member = internalMember //getter
    def member_=(value: Int): Unit = internalMember = value //setter
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 // aMutableContainer.member_=(42)
}
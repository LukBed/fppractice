package pl.snipersoft.advanced.darksyntaxsugar

object SingleAbstractMethodPattern extends App {
  trait Action {
    def act(x: Int): Int
    def doIt(): Unit = println("General Kenobi")
  }

  val myAction: Action = (x: Int) => x + 1

  val myRunnable: Runnable = () => println("Hello")
}
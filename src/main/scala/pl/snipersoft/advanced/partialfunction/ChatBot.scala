package pl.snipersoft.advanced.partialfunction

object ChatBot extends App {
  val myBot: PartialFunction[String, String] = {
    case "Hello" => "Hello there"
    case "How are you?" => "I'm fine"
  }

  println("Hello, my friend")
  scala.io.Source.stdin.getLines().map(myBot).foreach(println)
}

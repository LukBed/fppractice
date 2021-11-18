package pl.snipersoft.catseffect.concurrency

import cats.effect.{IO, IOApp}
import pl.snipersoft.catseffect.utils.IoOps

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object ResourcesBracketPattern extends IOApp.Simple {
  class Connection(url: String) {
    def open(): IO[String] = IO(s"Opening connection to $url...").debug
    def close(): IO[String] = IO(s"Closing connection to $url...").debug
  }

  val asyncFetchUrlWithLeakingResources: IO[Unit] = for {
    fib <- (new Connection("snipersoft.net").open() *> IO.sleep(Int.MaxValue.seconds)).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  val correctAsyncFetchUrl: IO[Unit] = for {
    conn <- IO(new Connection("snipersoft.net"))
    fib <- (conn.open() *> IO.sleep(Int.MaxValue.seconds)).onCancel(conn.close().void).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  //bracketPattern
  val bracketFetchUrl: IO[Unit] = IO(new Connection("snipersoft.net"))
    .bracket(conn => conn.open() *> IO.sleep(Int.MaxValue.seconds))(conn => conn.close().void) //releasing is executed after end of fiber or canceling
  val bracketProgram: IO[Unit] = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  override def run: IO[Unit] = bracketProgram
}

object BracketPatternExercises extends IOApp.Simple {

  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))


  /** Open scanner, read the file line by line, every 100 mls, close the scanner (if canceled or failed too) */
  def bracketReadFile(path: String): IO[Unit] = {
    openFileScanner(path).bracket(loop)(sc => IO("Closing scanner...").debug >> IO(sc.close())) //every line will be read by different thread
  }

  def loop(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug >> IO.sleep(100.millis) >> loop(scanner) else IO.unit

  def testWithCancel(path: String, duration: FiniteDuration): IO[Unit] = for {
    sc <- bracketReadFile(path).start
    _ <- IO.sleep(duration) >> sc.cancel.debug
  } yield ()

  val path = "src/main/scala/pl/snipersoft/catseffect/concurrency/Fibers.scala"

  override def run: IO[Unit] =
      bracketReadFile(path).void
//    testWithCancel(path, 4.seconds).void
}

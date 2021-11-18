package pl.snipersoft.catseffect.concurrency

import cats.effect.kernel.Outcome.Succeeded
import cats.effect.{IO, IOApp, Resource}
import pl.snipersoft.catseffect.utils.IoOps

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.{DurationInt, FiniteDuration}

//FP version of try-catch
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
    IO("Opening file").debug >> IO(new Scanner(new FileReader(new File(path))))

  /** Open scanner, read the file line by line, every 300 mls, close the scanner (if canceled or failed too) */
  def bracketReadFile(path: String): IO[Unit] = {
    openFileScanner(path).bracket(loop)(sc => IO("Closing scanner...").debug >> IO(sc.close())) //every line will be read by different thread
  }

  def loop(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) IO(scanner.nextLine()).debug >> IO.sleep(300.millis) >> loop(scanner) else IO.unit

  def testWithCancel(path: String, duration: FiniteDuration): IO[Unit] = for {
    sc <- bracketReadFile(path).start
    _ <- IO.sleep(duration) >> sc.cancel.debug
  } yield ()

  val path = "src/main/scala/pl/snipersoft/catseffect/concurrency/Fibers.scala"

  override def run: IO[Unit] =
    bracketReadFile(path).void
  //    testWithCancel(path, 4.seconds).void
}

object Resources extends IOApp.Simple {

  import ResourcesBracketPattern._
  import BracketPatternExercises._

  def connFromPath(path: String): IO[Unit] =
    openFileScanner(path).bracket { sc =>
      IO(new Connection(sc.nextLine())).bracket(conn => conn.open() >> IO.never)(conn => conn.close().void)
    }(sc => IO("Closing file...").debug >> IO(sc.close()))

  val connectionResource: Resource[IO, Connection] = Resource.make(IO(new Connection("snipersoft.net")))(con => con.close().void)

  val resourceFetchUrl: IO[Unit] = for {
    fib <- connectionResource.use(conn => conn.open() >> IO.never).start
    _ <- IO.sleep(2.seconds) >> fib.cancel
  } yield ()

  //to nesting resource use flatMap
  def nestedConnection(filePath: String): Resource[IO, Connection] =
  //    Resource.make(openFileScanner(filePath: String))(sc => IO(sc.close()) >> IO("Closing scanner...").debug.void)
  //    .flatMap(sc => Resource.make(IO(new Connection(sc.nextLine())))(con => con.close().void))
    for {
      sc <- Resource.make(openFileScanner(filePath: String))(sc => IO(sc.close()) >> IO("Closing scanner...").debug.void)
      conn <- Resource.make(IO(new Connection(sc.nextLine())))(con => con.close().void)
    } yield conn

  //connection with file will close automatically
  val connection: IO[Unit] = nestedConnection("src/main/resources/cats-effects/url.txt").use(conn => conn.open() >> IO.never)

  val testConnection: IO[Unit] = for {
    sc <- connection.start
    _ <- IO.sleep(2.seconds) >> sc.cancel
  } yield ()

  val ioWithFinalizer: IO[Unit] = IO("Some resource").debug.guarantee(IO("Freeing resource").debug.void).void
  val ioWithFinalizerV2: IO[Unit] = IO("Some resource").debug.guaranteeCase {
    case Succeeded(_) => IO("Freeing resource").debug.void
    case _ => IO("Freeing after error or cancellation").debug.void
  }.void

  override def run: IO[Unit] =
  //    resourceFetchUrl
  //    connection
  //    testConnection
    ioWithFinalizerV2
}

object ResourceExercise extends IOApp.Simple {

  import BracketPatternExercises._

  def getResourceFromFile(path: String): Resource[IO, Scanner] =
    Resource.make(openFileScanner(path))(sc => IO("Closing scanner...").debug >> IO(sc.close()))

  def resourceReadFile(path: String): IO[Unit] = {
    getResourceFromFile(path).use(loop)
  }

  def testWithCancel(path: String, duration: FiniteDuration): IO[Unit] = for {
    sc <- resourceReadFile(path).start
    _ <- IO.sleep(duration) >> sc.cancel.debug
  } yield ()

  override def run: IO[Unit] =
    resourceReadFile(path)
  //    testWithCancel(path, 4.seconds)
}
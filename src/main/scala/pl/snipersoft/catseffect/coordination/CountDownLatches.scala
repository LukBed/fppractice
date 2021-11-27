package pl.snipersoft.catseffect.coordination

import cats.effect.std.CountDownLatch
import cats.effect.{Deferred, IO, IOApp, Ref, Resource}
import pl.snipersoft.catseffect.utils.{IoOps, ioSleepMaxSecond}

import java.io.{File, FileWriter}
import scala.concurrent.duration.DurationInt
import scala.io.Source
import scala.util.Random

/**
 * CDLs are coordination primitives initialized with a count.
 * All fibers calling await() on the CDL are semantically blocked.
 * When the internal count of the latch reaches 0 (via release() method calls from other fibers),
 * all waiting fibers are unblocked.
 */
object CountDownLatches extends IOApp.Simple {

  import cats.syntax.parallel._

  def announcer(latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO("Starting race shortly...").debug >> IO.sleep(2.seconds)
    _ <- IO("5...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("4...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("3...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("2...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("1...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("GO!").debug
  } yield ()

  def runner(id: Int, latch: CountDownLatch[IO]): IO[Unit] = IO(s"[$id] Waiting for signal...").debug >>
    latch.await >> //blocks the fiber until the count reaches 0
    IO(s"[$id] Running").debug.void

  val sprint: IO[Unit] = for {
    latch <- CountDownLatch[IO](5)
    announcerFib <- announcer(latch).start
    runnersFib <- (1 to 10).toList.parTraverse(id => runner(id, latch)).start
    _ <- announcerFib.join
    _ <- runnersFib.join
  } yield ()

  override def run: IO[Unit] = sprint
}

object FileDownloaderExercise extends IOApp.Simple {

  import cats.syntax.parallel._
  import cats.syntax.traverse._

  object FileServer {
    val fileChunks = List("I love Scala", "Cats Effects are great!", "Pure functional programming", "Monads")

    def getNumberOfChunks: IO[Int] = IO(fileChunks.length)

    def getFileChunk(n: Int): IO[String] = IO(fileChunks(n))
  }

  def writeToFile(path: String, content: String): IO[Unit] = {
    val fileResource = Resource.make(IO(new FileWriter(new File(path))))(writer => IO(writer.close()))
    fileResource.use { writer =>
      IO(writer.write(content))
    }
  }

  def appendFileContents(fromPath: String, toPath: String): IO[Unit] = {
    val compositeResource = for {
      reader <- Resource.make(IO(Source.fromFile(fromPath)))(source => IO(source.close()))
      writer <- Resource.make(IO(new FileWriter(new File(toPath), true)))(writer => IO(writer.close()))
    } yield (reader, writer)

    compositeResource.use {
      case (reader, writer) => IO(reader.getLines().foreach(writer.write))
    }
  }

  def downloadFile(fileName: String, destinationFolder: String): IO[Unit] = {
    def tempFileName(n: Int): String = s"$destinationFolder/$fileName.part$n"

    def downloadTempFile(n: Int, latch: CountDownLatch[IO]): IO[Unit] = for {
      _ <- IO(s"Downloading chunk $n").debug
      _ <- ioSleepMaxSecond
      chunk <- FileServer.getFileChunk(n)
      - <- writeToFile(tempFileName(n), chunk)
      _ <- IO(s"Downloading chunk $n completed").debug
      _ <- latch.release
    } yield ()

    for {
      chunks <- FileServer.getNumberOfChunks
      latch <- CountDownLatch[IO](chunks)
      _ <- IO(s"Downloading started on $chunks fibers").debug
      _ <- (0 until chunks).toList.parTraverse(downloadTempFile(_, latch))
      _ <- latch.await
      //      >> appendFileContents(destinationFolder, s"$destinationFolder/$fileName")
      _ <- (0 until chunks).toList.traverse(n => appendFileContents(tempFileName(n), s"$destinationFolder/$fileName"))
    } yield ()
  }

  val path: String = "src/main/resources"

  override def run: IO[Unit] = downloadFile("result.txt", path)
}

object MyCountDownLatch extends IOApp.Simple {

  import cats.syntax.parallel._

  abstract class MyCountDownLatch {
    def release: IO[Unit]

    def await: IO[Unit]
  }

  object MyCountDownLatch {
    sealed trait State
    case object Done extends State
    case class Live(remainingCount: Int, signal: Deferred[IO, Unit]) extends State

    def apply(n: Int): IO[MyCountDownLatch] = for {
      signal <- IO.deferred[Unit]
      state <- Ref[IO].of[State](Live(n, signal))
    } yield new MyCountDownLatch {
      override def release: IO[Unit] = state.modify {
        case Done => Done -> IO.unit
        case Live(1, signal) => Done -> signal.complete(()).void
        case Live(remainingCount, signal) => Live(remainingCount - 1, signal) -> IO.unit
      }.flatten.uncancelable

      override def await: IO[Unit] = state.get.flatMap {
        case Done => IO.unit
        case _ => signal.get
      }
    }
  }

  override def run: IO[Unit] = for {
    myLatch <- MyCountDownLatch(5)
    _ <- (1 to 5).toList.parTraverse(n => ioSleepMaxSecond >> myLatch.release >> IO(s"Released $n").debug).start
    fib <- (myLatch.await >> IO("I'm ready!").debug).start
    _ <- fib.join
  } yield ()
}
import cats.effect._
import cats.syntax.parallel._
import scala.io._

object EffectsContinued extends IOApp {
// F:/input.txt

  private def getFile(blocker: Blocker)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[BufferedSource] =
    blocker.delay {
      println(s"getSeed - ${Thread.currentThread().getName}")
      println("Enter file path")
      Source.fromFile(scala.io.StdIn.readLine())
    }.handleErrorWith(_ => IO(println("Invalid file")) *> getFile(blocker))

  private def getWords(bufferedSource: BufferedSource): IO[List[String]] =
    IO { bufferedSource.getLines().toList.flatMap(_.trim.replaceAll("[.,]+", "").split(" +")).filterNot(_.isEmpty) }

  private def getSeed(blocker: Blocker)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[Int] =
    blocker.delay {
      println(s"getSeed - ${Thread.currentThread().getName}")
      println("Enter seed")
      scala.io.StdIn.readLine().toInt
    }.handleErrorWith(_ => IO(println("Invalid seed")) *> getSeed(blocker))

  private def javaHash(word: String, seed: Int = 0): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt
    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }
  private def knuthHash(word: String, constant: Int): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }

  case class Signature(file: String, seed: Int, hashes: List[Int])

  private def getSignature(file: BufferedSource, words: List[String], seed: Int): IO[Signature] =
    for {
      hashes <- words
        .parTraverse(word => IO { javaHash(word, seed) })
        .parProduct(words.parTraverse(word => IO { knuthHash(word, seed) }))
      (h1, h2) = hashes
    } yield Signature(file.descr, seed, List(h1.min, h2.min))

  override def run(args: List[String]): IO[ExitCode] =
    for {
      file      <- Blocker[IO].use(getFile)
      seed      <- Blocker[IO].use(getSeed)
      words     <- getWords(file)
      signature <- getSignature(file, words, seed)
      _         <- IO(println(signature))
    } yield ExitCode.Success

}

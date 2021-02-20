import sbt._
import Keys._

object BulkySourcesPlugin extends AutoPlugin {

  object autoImport {
    val bulkyThresholdInLines = settingKey[Int]("bulky threshold in lines")
    val bulkySources = taskKey[Seq[(Int, File)]]("bulky sources")
  }

  def getBulkyLines(files: Seq[File], threshold: Int): Seq[(Int, File)] = {
    files
      .map(file => (sbt.IO.readLines(file).size, file))
      .sortBy { case (x, _) => x }(Ordering[Int].reverse)
      .filter { case (x, _) => x >= threshold }
  }

  import autoImport._

  override val projectSettings: Seq[Setting[_]] = Seq(
    bulkyThresholdInLines := 100,
    bulkySources := getBulkyLines(
      (Compile / sources).value,
      bulkyThresholdInLines.value
    ),
    (Test / bulkySources) := getBulkyLines(
      (Test / sources).value,
      bulkyThresholdInLines.value
    )
  )

}

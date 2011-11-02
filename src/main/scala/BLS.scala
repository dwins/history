import org.geoscript._

object BLS extends App {
  require(args.size > 0, "Please tell me where the data is")
  val file = new java.io.File(args(0))
  require(file.isDirectory,
    "%s can't be found or isn't a directory (full path: %s)".format(file, file.getAbsolutePath()))

  val ws = workspace.Directory(file)
  val data = ws.layer(ws.names.head)

  val rates = data.features.map(_.get[Double]("BLS_Rate"))
  println(rates.min, rates.max)
  val groups = rates.groupBy(math.round)
    .mapValues(_.size)
    .toSeq.sortBy(_._1)

  groups foreach println
}

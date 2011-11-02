import org.geoscript._

object BLS extends App {
  val ws = workspace.Directory("/home/dwins/opt/mapstory/MapStory_BLS/")
  val data = ws.layer(ws.names.head)

  val rates = data.features.map(_.get[Double]("BLS_Rate"))
  println(rates.min, rates.max)
  val groups = rates.groupBy(math.round)
    .mapValues(_.size)
    .toSeq.sortBy(_._1)

  groups foreach println
}

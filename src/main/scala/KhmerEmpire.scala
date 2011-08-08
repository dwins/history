import org.geoscript._
import style.combinators._
import org.opengis.filter.Filter
import org.geotools.filter.text.ecql.ECQL.{ toFilter => cql }

object KhmerData {
  lazy val density = layer.Shapefile("/home/dwins/opt/khmer/gis_data/density.shp")
  lazy val polygons = layer.Shapefile("/home/dwins/opt/khmer/gis_data/polygons.shp")
  lazy val animation = layer.Shapefile("/home/dwins/opt/khmer/gis_data/animation_points.shp")
}

object KhmerEmpire extends App {
  import KhmerData._

  def join(years: Seq[Long], frames: Seq[Long]): Seq[(Long, Filter, Filter)] = {
    def framesFor(year: Long) = 
      frames.filter { 
        frame => years.minBy { y => math.abs(frame - y) } == year 
      }

    for {
      year <- years
      frames = framesFor(year)
    } yield
      Triple(
        year,
        cql("YEAR = " + year.toString),
        cql(
          if (frames.isEmpty) "EXCLUDE"
          else frames.mkString("YEAR = ", " OR YEAR = ", "")
        )
      )
  }

  val empire = (
    (Fill("#000000") where cql("EMPIRE_NAM = 'Ayutthaya'")) and
    (Fill("#0000FF") where cql("EMPIRE_NAM = 'Phayao'")) and
    (Fill("#00FF00") where cql("EMPIRE_NAM = 'Laos'")) and
    (Fill("#00FFFF") where cql("EMPIRE_NAM = 'Sukhtothai'")) and
    (Fill("#FF0000") where cql("EMPIRE_NAM = 'China'")) and
    (Fill("#FF00FF") where cql("EMPIRE_NAM = 'Chenla'")) and
    (Fill("#FFFF00") where cql("EMPIRE_NAM = 'Haripunjaya'")) and
    (Fill("#FFFFFF") where cql("EMPIRE_NAM = 'Lanna'")) and
    (Fill("#0000AA") where cql("EMPIRE_NAM = 'Dvaravati'")) and
    (Fill("#00AA00") where cql("EMPIRE_NAM = 'Sukhothai'")) and
    (Fill("#00AAAA") where cql("EMPIRE_NAM = 'Funan'")) and
    (Fill("#AA0000") where cql("EMPIRE_NAM = 'Champa'")) and
    (Fill("#AA00AA") where cql("EMPIRE_NAM = 'Kambujadesa'"))
  )

  val markers =
    Symbol("star", 
      size=10, fill=Fill("#FFFE74"), stroke=Stroke("#000000"))

  val times =
    polygons.features.map(_.get[Long]("YEAR")).toList.distinct.sorted

  val polyList = polygons.features.toList.sortBy { f => (f.get[Long]("YEAR"), f.get[Long]("EMPIRE_ID")) }
  val densList = density.features.toList.sortBy { f => (f.get[Long]("YEAR"), f.get[Long]("EMPIRE_ID")) }

  for ((p, d) <- polyList zip densList) {
    println(p.properties.values.filterNot { _.isInstanceOf[geometry.Geometry] })
    println(d.properties.values.filterNot { _.isInstanceOf[geometry.Geometry] })
  }

  // val ram = workspace.Memory()
  // val rendering = ram.create("render", polygons.schema.fields: _*)
  // val viewport = render.Viewport(polygons.bounds)

  // for (t <- times) {
  //   val toAdd = polygons.features.filter(_.get[Long]("YEAR") == t).toList
  //   val empires = toAdd.map { _.get[Long]("EMPIRE_ID") }.toSet
  //   rendering --= rendering.features.filter(f => empires(f.get[Long]("EMPIRE_ID")))
  //   rendering ++= toAdd

  //   val destination = render.PNG("img/khmer_polygon_%04d.png".format(t)) _
  //   viewport.render(Seq(
  //     rendering -> empire
  //   )).writeTo(destination)
  // }

  // val joinedTimes = join(times.toSeq.sorted, frames.toSeq.sorted)
  // joinedTimes foreach println
  // val viewport = render.Viewport(polygons.bounds)
  // for ((year, inTime, inFrame) <- joinedTimes.par) {
  //   val destination = render.PNG("img/khmer_polygon_%04d.png".format(year)) _
  //   viewport.render(Seq(
  //     polygons -> (empire where inTime),
  //     animation -> (markers where inFrame)
  //   )).writeTo(destination)
  // }

  // val grouped = polygons.features.groupBy(_.get[Long]("YEAR"))
  // println(polygons.schema)
  // grouped
  //  .mapValues { _ map { _.get[Long]("EMPIRE_ID") } }
  //  .toList
  //  .sortBy { _._1 } 
  //  .foreach { println }
}

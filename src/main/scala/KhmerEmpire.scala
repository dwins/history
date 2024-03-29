import org.geoscript._
import feature.{ Feature, Field } 
import style.combinators._
import org.opengis.filter.Filter
import org.geotools.filter.text.ecql.ECQL.{ toFilter => cql }

object KhmerEmpire extends App {
  require(args.size > 0, "Please tell me where the khmer data is")
  val file = new java.io.File(args(0))
  require(file.isDirectory,
    "%s can't be found or isn't a directory (full path: %s)".format(file, file.getAbsolutePath()))

  val khmer = workspace.Directory(args(0))
  val density = khmer.layer("density")
  val polygons = khmer.layer("polygons")
  val animation = khmer.layer("animation")


  val times =
    polygons.features.map(_.get[Long]("YEAR")).toList.distinct.sorted

  val polyList = polygons.features.toList.sortBy {
    f => (f.get[Long]("YEAR"), f.get[Long]("EMPIRE_ID"))
  }

  val densList = density.features.toList.sortBy {
    f => (f.get[Long]("YEAR"), f.get[Long]("EMPIRE_ID"))
  }

  var buffer = Seq.empty[Feature]
  val rangedPolygons = polygons.workspace.create("rangedpolygons", 
    polygons.schema.fields.filter(_.name != "YEAR") ++ 
    Seq(Field("START_YEAR", classOf[java.lang.Long]), Field("END_YEAR", classOf[java.lang.Long]))
  )
 
  val timedAnimation = animation.workspace.create("timedanimation",
    animation.schema.fields.filter(_.name != "YEAR") :+ Field("YEAR", classOf[java.util.Date])
  )

  def makeRanged(f: Feature, end: java.lang.Long): Feature = {
    val props = 
      f.properties.map {
        case ("YEAR", v) => ("START_YEAR", v)
        case x => x
      }
    Feature(props + ("END_YEAR" -> end)) 
  }

  for (t <- times) {
    val toAdd = polygons.features.filter(_.get[Long]("YEAR") == t).toList
    val byEmpire = toAdd map { f => (f.get[Long]("EMPIRE_ID") -> f) } toMap
    val invalidatedEmpires = buffer.collect {
      case f if byEmpire contains f.get[Long]("EMPIRE_ID") =>
        (f, byEmpire(f.get[Long]("EMPIRE_ID")))
    }

    rangedPolygons ++= invalidatedEmpires map { 
      case (start, end) => makeRanged(start, end.get("YEAR"))
    }

    buffer = buffer.filterNot { f => byEmpire.contains(f.get[Long]("EMPIRE_ID")) }
    buffer ++= toAdd
  }

  rangedPolygons ++= buffer.view.map { makeRanged(_, null) } 

  timedAnimation ++= animation.features map { f => 
    Feature(f.properties + (
      "YEAR" -> new java.util.Date(f.get[Double]("YEAR").toInt, 0, 1, 0, 0, 0),
      "DURATION" -> new java.util.Date(f.get[Double]("DURATION").toInt, 0, 1, 0, 0, 0)
    ))
  }
}

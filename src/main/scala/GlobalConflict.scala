import org.geoscript._

object GlobalConflict extends App {
  import au.com.bytecode.opencsv
  import java.io.{ BufferedReader, InputStreamReader }
  import geometry._, feature._
  type Row = Seq[String]

  def rows(in: java.io.InputStream): Iterator[Row] = {
    val reader =
      new opencsv.CSVReader(new BufferedReader(new InputStreamReader(in)))
    Iterator.continually(reader.readNext: Row).takeWhile { x =>
      if (x == null) { reader.close(); false } else true
    }
  }

  val sites = rows(getClass.getResourceAsStream("ConflictSite 4-2006_v2.csv")).drop(1).toIndexedSeq
  val main = rows(getClass.getResourceAsStream("Main Conflict Table.csv")).drop(1).toIndexedSeq
  val ws = workspace.Directory(".")

  val layer = ws.create("conflicts", 
    Field("ID",  classOf[java.lang.Integer]),
    Field("Site",  classOf[String]),
    Field("SideA",  classOf[String]),
    Field("SideA2nd",  classOf[String]),
    Field("SideB",  classOf[String]),
    Field("SideB2nd",  classOf[String]),
    Field("Incomp",  classOf[String]),
    Field("Terr",  classOf[String]),
    Field("YEAR",  classOf[java.lang.Integer]),
    Field("Int",  classOf[String]),
    Field("CumInt",  classOf[String]),
    Field("Type",  classOf[String]),
    Field("StartDate",  classOf[java.util.Date]),
    Field("StartPrec",  classOf[String]),
    Field("StartDate2",  classOf[java.util.Date]),
    Field("Startprec2",  classOf[java.lang.Integer]),
    Field("EpEnd",  classOf[java.lang.Integer]),
    Field("EpEndDate",  classOf[java.util.Date]),
    Field("EpEndPrec",  classOf[java.lang.Integer]),
    Field("GWNOA",  classOf[java.lang.Integer]),
    Field("GWNOA2nd",  classOf[java.lang.Integer]),
    Field("GWNOB",  classOf[java.lang.Integer]),
    Field("GWNOB2nd",  classOf[java.lang.Integer]),
    Field("GWNOLoc",  classOf[java.lang.Integer]),
    Field("Region",  classOf[java.lang.Integer]),
    Field("Version",  classOf[String]),
    Field("radius",  classOf[java.lang.Double]),
    Field("the_geom",  classOf[geometry.MultiPoint], "EPSG:4326")
  )

  val format = new java.text.SimpleDateFormat("MM/dd/yyyy")
  def date(s: String): java.util.Date =
    util.control.Exception.allCatch.opt(format.parse(s)).getOrElse(null)

  layer ++= main map { event => 
      val id = event(0)
      val year = event(8)
      val sitesForEvent = 
        sites.filter(s => id == s(0) && year == s(1))
          .map { _.view map (_ toDouble) }
      val locations = sitesForEvent.map (d => Point(d(3), d(2)))
      val radius = sitesForEvent.map(_(4)).sum / sites.size
      val geom = MultiPoint(locations: _*)
      Feature(
        "ID" -> event(0),
        "Site" -> event(1),
        "SideA" -> event(2),
        "SideA2nd" -> event(3),
        "SideB" -> event(4),
        "SideB2nd" -> event(5),
        "Incomp" -> event(6),
        "Terr" -> event(7),
        "YEAR" -> new java.util.Date(year.toInt, 0, 0),
        "Int" -> event(9),
        "CumInt" -> event(10),
        "Type" -> event(11),
        "StartDate" -> date(event(12)),
        "StartPrec" -> event(13),
        "StartDate2" -> date(event(14)),
        "Startprec2" -> event(15),
        "EpEnd" -> event(16),
        "EpEndDate" -> date(event(17)),
        "EpEndPrec" -> event(18),
        "GWNOA" -> event(19),
        "GWNOA2nd" -> event(20),
        "GWNOB" -> event(21),
        "GWNOB2nd" -> event(22),
        "GWNOLoc" -> event(23),
        "Region" -> event(24),
        "Version" -> event(25),
        "radius" -> radius,
        "the_geom" -> geom
      )
    }
}

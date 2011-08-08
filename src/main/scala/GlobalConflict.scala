import org.geoscript._

object GlobalConflict extends App {
  import java.io.File
  import au.com.bytecode.opencsv
  def rows(s: String): Iterator[Seq[String]] = rows(new java.io.File(s))
  def rows(f: File): Iterator[Seq[String]] = {
    val reader = new opencsv.CSVReader(new java.io.BufferedReader(new java.io.FileReader(f)))
    Iterator.continually(reader.readNext: Seq[String]).takeWhile { x =>
      if (x == null) { reader.close(); false } else true
    }
  }

  type Row = Seq[String]

  val conflictSites = rows("ConflictSite 4-2006_v2.csv").toSeq
  val mainYears = rows("Main Conflict Table.csv").toSeq

  trait Converter[A] {
    def binding: Class[A]
    def coerce(s: String): Option[A]
  }

  object Converters {
    import java.lang.{String => JString}
    import util.control.Exception.allCatch
    private abstract class Impl[A](val binding: Class[A]) extends Converter[A]

    val String: Converter[String] = new Impl(classOf[JString]) {
      def coerce(s: JString): Option[JString] = Option(s).filterNot(_.isEmpty)
    }

    val Int: Converter[Int] = new Impl(classOf[Int]) {
      def coerce(s: JString): Option[Int] = allCatch.opt(s.toInt)
    }

    val Double: Converter[Double] = new Impl(classOf[Double]) {
      def coerce(s: JString): Option[Double] = allCatch.opt(s.toDouble)
    }

    val Date: Converter[java.util.Date] = new Impl(classOf[java.util.Date]) {
      def coerce(s: JString): Option[java.util.Date] =
        allCatch.opt(new java.text.SimpleDateFormat("MM/dd/yyyy").parse(s))
    }
  }

  import geometry._, feature._

  val sites = rows("ConflictSite 4-2006_v2.csv").drop(1).toIndexedSeq
  val main = rows("Main Conflict Table.csv").drop(1).toIndexedSeq
  val ws = workspace.Directory(".")

  val layer = ws.create("conflicts", 
    Field("ID",  classOf[java.lang.Integer]),
    Field("Location",  classOf[String]),
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
    Field("the_geom",  classOf[geometry.MultiPolygon], "EPSG:4326")
  )

  val format = new java.text.SimpleDateFormat("MM/dd/yyyy")
  def date(s: String): java.util.Date =
    util.control.Exception.allCatch.opt(format.parse(s)).getOrElse(null)

  layer ++= (
    for (event <- main) yield {
      val id = event(0)
      val year = event(8)
      val locations =
        sites
         .filter(s => id == s(0) && year == s(1))
         .map { s =>
           val d = s.view.map(_.toDouble); 
           Point(d(3), d(2)).buffer(d(4) / 1000d).asInstanceOf[Polygon]
         }
      if (locations.size > 0) println(locations.size)
      val geom = MultiPolygon(locations)
      Feature(
        "ID" -> event(0),
        "Location" -> event(1),
        "SideA" -> event(2),
        "SideA2nd" -> event(3),
        "SideB" -> event(4),
        "SideB2nd" -> event(5),
        "Incomp" -> event(6),
        "Terr" -> event(7),
        "YEAR" -> event(8),
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
        "the_geom" -> geom
      )
    }
  )
}

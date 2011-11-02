import org.geoscript._

object HarvardAttendance extends App {
  val dataDir = "/home/dwins/opt/mapstory/harvard_international_attendance/"

  val attendanceData = {
    import au.com.bytecode.opencsv._
    import scala.collection.JavaConversions._
    import java.io._
    val csvFile = 
      new File(dataDir + "multiannstbyschool91-11students.csv")
    val reader = new CSVReader(new FileReader(csvFile))
    val lines = reader.readAll().drop(2) // title and blank spacer line
    val header = lines.head
    val data = lines.tail
    data map (row => (header zip row).toMap)
  }

  val data = workspace.Directory(dataDir)
  val reference = workspace.Directory("/home/dwins/opt/mapstory/reference/")

  val neCountries = reference.layer("ne_10m_admin_0_countries")
  val tmBorders = reference.layer("TM_WORLD_BORDERS-0")

  val corrections = Map(
    "WB" -> 275,
    "SR" -> 688,
    "UR" -> 643,
    "TC" -> 784,
    "YE" -> 887,
    "YO" -> 688
  )

  val result = data.create("harvard_attendance",
    feature.Field("year", classOf[java.util.Date]),
    feature.Field("students", classOf[java.lang.Integer]),
    feature.Field("co_iap", classOf[String]),
    feature.Field("co_code", classOf[java.lang.Integer]),
    feature.Field("co_name", classOf[String]),
    feature.Field("the_geom", classOf[geometry.Point], tmBorders.schema.geometry.projection),
    feature.Field("co_wb", classOf[String]))

  val years = {
    val semesters = Seq(
      "91-92", "92-93", "93-94", "94-95", "95-96", "96-97", "97-98", "98-99",
      "99-00", "00-01", "01-02", "02-03", "03-04", "04-05", "05-06", "06-07",
      "07-08", "08-09", "09-10", "10-11"
    )
    def date(x: Int) = new java.util.Date(x, 0, 0)
    Stream.from(92).map(date) zip semesters
  }

  val geometriesByUN =
    tmBorders.features.map { x =>
      (x.get[Int]("UN"), x.get[geometry.MultiPolygon]("the_geom"))
    }.toMap
  val geometriesByFIPS =
    tmBorders.features.map { x => 
      (x.get[String]("FIPS"), x.get[geometry.MultiPolygon]("the_geom"))
    }.toMap

  def geomFor(row: Map[String, String]): geometry.MultiPolygon = {
    val countryCode = row("country_iap_code")
    if (corrections contains countryCode)
      geometriesByUN(corrections(countryCode))
    else
      geometriesByFIPS(countryCode)
  }
        
  result ++= (
    for {
      row <- attendanceData.view
      if row("Country of Origin") != "Totals by Year"
      geom = geomFor(row)
      (year, semester) <- years
    } yield {
      feature.Feature(
        "year" -> year,
        "students" -> row(semester),
        "co_iap" -> row("country_iap_code"),
        "co_code" -> row("country_code"),
        "co_name" -> row("Country of Origin"),
        "co_wb" -> row("country_world_bank"),
        "the_geom" -> geom.centroid
      )
    }
  )
} 

import org.geoscript._
object ChineseSocialNetworks extends App {
  import au.com.bytecode.opencsv, java.io._

  // not all the files we got were in GBK encoding
  val encodings = 
    Map(
      "EXAM_1060-1089.txt" -> "UTF-8",
      "kin_gis_ZhaoKuangyin_5_10_1_1_Allan.txt" -> "UTF-8",
      "Kin  Lv Yijian.txt" -> "ISO-8859-1"
    )

  val needsBOMEaten = Set("kin_gis_ZhaoKuangyin_5_10_1_1_Allan.txt")

  val toSkip = Set("Kin  Lv Yijian.txt")

  def dateFromYear(year: Int) = 
    if (year <= 1) null 
    else new java.util.Date(year - 1900, 0, 1)

  def load(f: File): (Seq[String], Seq[Seq[String]]) = {
    val fin = new FileInputStream(f)
    val fread = new InputStreamReader(fin, encodings.getOrElse(f.getName, "GBK"))
    if (needsBOMEaten(f.getName)) fread.read()
    val reader = new opencsv.CSVReader(fread)
    val rows = Iterator.continually(reader.readNext).takeWhile(_ != null).toList
    (rows.head, rows.tail.map(_.toList))
  }

  val kinLayers = Map(
    "kin_gis_ZhaoKuangyin_5_10_1_1_Allan.txt" -> "zhao_kuangyin",
    "chenghaokin_10_10_1_1.txt" -> "cheng_haokin", 
    "Kin  Lv Yijian.txt" -> "lv_yijian",
    "Direct Descendants of Zhao Jiong-Emp Taizong.txt" -> "zhao_jiong", 
    "Kin_Wang Anshi.txt" -> "wang_anshi",
    "kin_Qin Gui.txt" -> "qin_gui",
    "zhuxikin_gis.txt" -> "zhu_xi",
    "Lv MenqiGBK111013.txt" -> "lv_menqi"
  )

  import org.geoscript.feature.Field
  val kinFields = 
    Seq(
      Field("name", classOf[String]),
      Field("namechn", classOf[String]),
      Field("indexyear", classOf[java.util.Date]),
      Field("sex", classOf[String]),
      Field("relation", classOf[String]),
      Field("up", classOf[String]),
      Field("down", classOf[String]),
      Field("marriage", classOf[String]),
      Field("collateral", classOf[String]),
      Field("addrname", classOf[String]),
      Field("addrchn", classOf[String]),
      Field("the_geom", classOf[org.geoscript.geometry.Point], "EPSG:4326"),
      Field("xy_count", classOf[java.lang.Integer])
    )

  val examFields = 
    Seq(
      Field("name", classOf[String]),
      Field("namechn", classOf[String]),
      Field("indexyear", classOf[java.util.Date]),
      Field("entrydesc", classOf[String]),
      Field("entrychn", classOf[String]),
      Field("entryyear", classOf[java.util.Date]),
      Field("entryrank", classOf[String]),
      Field("kintype", classOf[String]),
      Field("kinname", classOf[String]),
      Field("kinchn", classOf[String]),
      Field("addrname", classOf[String]),
      Field("addrchn", classOf[String]),
      Field("the_geom", classOf[org.geoscript.geometry.Point], "EPSG:4326"),
      Field("xy_count", classOf[String])
    )

  val dataDir = new File("/home/dwins/opt/mapstory/chinese_social_data/")
  val workspace = org.geoscript.workspace.Workspace(
    "url" -> new File(dataDir, "shapefiles/").toURI.toURL,
    ("charset" -> java.nio.charset.Charset.forName("UTF-8")).asInstanceOf[(String, Serializable)]
  )

  for {
    file <- dataDir.listFiles.toSeq
    if !file.isDirectory
    (header, data) = load(file)
    badRow <- data.find(_.size < header.size)
  } {
    println("%s has incomplete row: \n%s".format(file, header zip badRow))
    println("here is a sample row:  \n%s".format(header zip data.head))
  }

  val exams = workspace.create("exams", examFields: _*)

  for {
    file <- dataDir.listFiles.toSeq
    if !file.isDirectory && !toSkip(file.getName)
    (header, data) = load(file)
  } {
    kinLayers.get(file.getName) match {
      case Some(name) =>
        val expected = Seq("Name", "NameChn", "IndexYear", "Sex", "Relation", "Up", "Down", "Marriage", "Collateral", "AddrName", "AddrChn", "X", "Y", "XY_count")
        require(header == expected,
        "Unexpected header for kin layer: %s; %s".format(file.getName, header zip expected filter { case (a, b) => a != b }))
        val layer = workspace.create(name, kinFields: _*)
        try {
          layer ++= data.flatMap { row =>
            try {
              val year = dateFromYear(row(2).trim.toInt)
              if (year == null)
                None
              else
                Some(org.geoscript.feature.Feature(
                  "name" -> row(0),
                  "namechn" -> row(1),
                  "indexyear" -> year,
                  "sex" -> row(3),
                  "relation" -> row(4),
                  "up" -> row(5),
                  "down" -> row(6),
                  "marriage" -> row(7),
                  "collateral" -> row(8),
                  "addrname" -> row(9),
                  "addrchn" -> row(10),
                  "the_geom" -> org.geoscript.geometry.Point(row(11).trim.toDouble, row(12).trim.toDouble),
                  "xy_count" -> row(13).trim.toInt
                ))
            } catch { case ex => sys.error("Bad row is %s; error was %s".format(row, ex)) }
          }
        } catch { case ex => println("%s during processing of %s".format(ex, file.getName)); ex.printStackTrace }
      case None =>
        require(header == Seq("Name", "NameChn", "IndexYear", "EntryDesc", "EntryChn", "EntryYear", "EntryRank", "KinType", "KinName", "KinChn", "AddrName", "AddrChn", "X", "Y", "xy_count"),
          "Unexpected header for exam layer: " + file.getName)
        try {
          exams ++= data.flatMap { row =>
            try {
              val year = dateFromYear(row(2).trim.toInt)
              if (year != null) 
                None
              else 
                Some(
                  if (row.size == header.size) 
                    org.geoscript.feature.Feature(
                      "name" -> row(0),
                      "namechn" -> row(1),
                      "indexyear" -> dateFromYear(row(2).trim.toInt),
                      "entrydesc" -> row(3),
                      "entrychn" -> row(4),
                      "entryyear" -> dateFromYear(row(5).trim.toInt),
                      "entryrank" -> row(6),
                      "kintype" -> row(7),
                      "kinname" -> row(8),
                      "kinchn" -> row(9),
                      "addrname" -> row(10),
                      "addrchn" -> row(11),
                      "the_geom" -> org.geoscript.geometry.Point(row(12).trim.toDouble, row(13).trim.toDouble),
                      "xy_count" -> row(14)
                    )
                  else
                    org.geoscript.feature.Feature(
                      "name" -> row(0),
                      "namechn" -> row(1),
                      "indexyear" -> dateFromYear(row(2).trim.toInt),
                      "entrydesc" -> (row(3) + row(4)),
                      "entrychn" -> row(5),
                      "entryyear" -> dateFromYear(row(6).trim.toInt),
                      "entryrank" -> row(7),
                      "kintype" -> row(8),
                      "kinname" -> row(9),
                      "kinchn" -> row(10),
                      "addrname" -> row(11),
                      "addrchn" -> row(12),
                      "the_geom" -> org.geoscript.geometry.Point(row(13).trim.toDouble, row(14).trim.toDouble),
                      "xy_count" -> row(15)
                    )
                )
            } catch { case ex => sys.error("Bad row is %s; error was %s".format(row, ex)) }
          }
        } catch { case ex => println("%s during processing of %s".format(ex, file.getName)) }
    }
  }
}

import org.geoscript._
import feature.{ Feature, Field, Schema }

object IreneAnimation extends App {
  val irene = workspace.Directory("/home/dwins/opt/mapstory/irene/")

  val paths = irene.layer("irene_11_lin")
  val points = irene.layer("irene_11_pts")

  def heading(dx: Double, dy: Double): Double = {
    import math.{ atan2, toDegrees }
    val angle = toDegrees(atan2(dy, dx))
    if (angle < 90d)
      - angle
    else
      360d - angle
  }

  def direction(l: geometry.LineString): Double = {
    val start = l.vertices.init.last
    val end = l.vertices.last
    heading(end.x - start.x, end.y - start.y)
  }

  def mDirection(l: geometry.MultiLineString): Double = 
    direction(l.members.first)

  val segments = paths.features.flatMap { f =>
    val geom = f.get[geometry.MultiLineString]("the_geom")
    geom.members.head.vertices.sliding(2).map {
      case Seq(a, b) => geometry.LineString(a, b)
    }
  }

  val segmentFeatures =
    for { 
      f <- paths.features.toSeq
      geom = f.get[geometry.MultiLineString]("the_geom").members.head
      Seq(a, b) <- geom.vertices.sliding(2)
      segment = geometry.MultiLineString(geometry.LineString(a, b))
    } yield f.update("the_geom" -> segment)

  val positions =
    for {
      Seq(a, b) <- points.features.toSeq.sliding(2).toSeq
      box = a.geometry.bounds expand b.geometry.bounds
      tracktime = a.get[String]("TRACKTIME")
      trackstamp = a.get[java.util.Date]("trackstamp")
    } yield (box, tracktime, trackstamp)

  val tracksAndTimes: Seq[(String, java.util.Date, Seq[Feature])] =
    for {
      (box, time, stamp) <- positions
      extras = Seq("TRACKTIME" -> time, "trackstamp" -> stamp, "opacity" -> 1d)
      updated =
        for {
          f <- segmentFeatures 
          if box intersects f.get[geometry.MultiLineString]("the_geom")
        } yield Feature((f.properties ++ extras).toSeq: _*)
    } yield (time, stamp, updated)

  val fadingOpacities = (10 to 0 by -2) map (_ / 10d)

  val chunks: Seq[Seq[(String, java.util.Date, Seq[Feature])]] =
    ((1 to 3) map (tracksAndTimes take _)) ++ (tracksAndTimes sliding 4)

  val timedTrackSchema = Schema(
    "irene_11_timed_tracks",
    paths.schema.fields
      :+ Field("TRACKTIME", classOf[String])
      :+ Field("trackstamp", classOf[java.util.Date])
      :+ Field("opacity", classOf[java.lang.Double])
  )

  val timedTracks = irene.create(timedTrackSchema)

  timedTracks ++= (
    for {
      chunk <- chunks
      (timespec, timestamp, _) = chunk.last
      (fs, opacity) <- chunk.map(_._3).reverse zip fadingOpacities
      f <- fs
    } yield
      f update (
        "TRACKTIME" -> timespec,
        "trackstamp" -> timestamp,
        "opacity" -> opacity
      )
  )
}

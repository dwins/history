name := "history"

organization := "me.winslow.d"

scalaVersion := "2.9.1"

fork in run := true

libraryDependencies ++= Seq(
  "org.geoscript" %% "geoscript" % "0.7.3",
  "net.sf.opencsv" % "opencsv" % "2.3"
)

resolvers ++= Seq(
  "opengeo" at "http://repo.opengeo.org/",
  "osgeo" at "http://download.osgeo.org/webdav/geotools/"
)

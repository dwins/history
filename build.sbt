name := "dateline"

organization := "me.winslow.d"

scalaVersion := "2.9.0-1"

libraryDependencies ++= Seq(
  "org.geoscript" %% "geoscript" % "[0,)",
  "net.sf.json-lib" % "json-lib" % "2.3" classifier "jdk15",
  "net.sf.opencsv" % "opencsv" % "2.3"
)

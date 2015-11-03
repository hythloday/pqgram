
scalaBinaryVersion := "2.11"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.assembla.scala-incubator" %% "graph-core" % "1.9.4",
  "com.assembla.scala-incubator" %% "graph-constrained" % "1.9.0",
  "com.assembla.scala-incubator" %% "graph-dot" % "1.10.0",
  "com.assembla.scala-incubator" %% "graph-test" % "1.9.0" % "test",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

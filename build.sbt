
scalaBinaryVersion := "2.11"

scalaVersion := "2.11.7"

name := "pqgram"

organization := "io.bimble"

version := "0.2-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.assembla.scala-incubator" %% "graph-core" % "1.9.4",
  "com.assembla.scala-incubator" %% "graph-constrained" % "1.9.0",
  "com.assembla.scala-incubator" %% "graph-dot" % "1.10.0",
  "com.assembla.scala-incubator" %% "graph-test" % "1.9.0" % "test",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)

pomExtra in Global := {
  <url>https://github.com/hythloday/pqgram</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      </license>
    </licenses>
    <scm>
      <connection>scm:git:github.com/hythloday/pqgram.git</connection>
      <developerConnection>scm:git:git@github.com:hythloday/pqgram.git</developerConnection>
      <url>github.com/hythloday/pqgram.git</url>
    </scm>
    <developers>
      <developer>
        <id>hythloday</id>
        <name>RC James Harlow</name>
      </developer>
    </developers>
}

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

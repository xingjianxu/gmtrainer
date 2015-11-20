import sbt.Keys._

name := "gmtrainer"

version := "1.0"
scalaVersion := "2.11.7"

mainClass in Compile := Some("cbb.gmtrainer.Main")

libraryDependencies ++= Seq(
  "org.biojava" % "biojava-sequencing" % "4.1.0",
  "org.biojava" % "biojava-genome" % "4.1.0",
  "com.google.guava" % "guava" % "19.0-rc2",
  "com.google.code.findbugs" % "jsr305" % "1.3.+",
  "org.scalanlp" %% "breeze" % "0.11.2",
  "org.scalanlp" %% "breeze-natives" % "0.11.2",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
).map(_.force())

libraryDependencies ~= { _.map(_.exclude("org.slf4j", "slf4j-log4j12")) }

resolvers += "CBB" at "http://cloud.big.ac.cn/mvn/content/groups/public/"
externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false)
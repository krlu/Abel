name := "MET-CS-789"

version := "1.0"

scalaVersion := "2.12.3"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.2.0-SNAP9" % "test"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.1.0"

scalacOptions += "-Ypartial-unification"

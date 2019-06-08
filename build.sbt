name := "Abel"

version := "1.0"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.2.0-SNAP9" % "test"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "cc.redberry" %% "rings.scaladsl" % "2.5.2"


scalacOptions += "-Ypartial-unification"

scalacOptions ++= Seq("-unchecked", "-deprecation")

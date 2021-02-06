name := "Abel"

version := "1.0"

scalaVersion := "2.12.11"

libraryDependencies ++=
  List(
    "org.scalatest" % "scalatest_2.12" % "3.2.0-SNAP9" % "test",
    "org.apache.commons" % "commons-math3" % "3.6.1",
    "cc.redberry" %% "rings.scaladsl" % "2.5.2",
    "com.chuusai" %% "shapeless" % "2.4.0-M1",
    "ch.obermuhlner" % "big-math" % "2.3.0"
  )


scalacOptions += "-Ypartial-unification"

scalacOptions ++= Seq("-unchecked", "-deprecation")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
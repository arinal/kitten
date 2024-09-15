import Dependencies._

ThisBuild / scalaVersion := "2.13.14"
ThisBuild / version := "0.1.5"
ThisBuild / organization := "lamedh"
ThisBuild / organizationName := "lamedh"

lazy val root   = (project in file("modules")).aggregate(kitten, scale)
lazy val exercise = (project in file("modules/exercise")).dependsOn(kitten, scale)
lazy val kitten = (project in file("modules/kitten")).dependsOn(scale)
lazy val scale  = (project in file("modules/scale"))

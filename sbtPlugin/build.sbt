import Dependencies._
sbtPlugin := true
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"
enablePlugins(BulkySourcesPlugin)

lazy val root = (project in file("."))
  .settings(
    name := "BulkySourcesPlugin",
    libraryDependencies += scalaTest % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

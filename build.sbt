import AssemblyKeys._ // put this at the top of the file

assemblySettings

mainClass in assembly := Some("classmapper.ClassMapperApp")

name := "ClassMapper"

version := "1.0"

organization := "test"

scalaVersion := "2.10.4"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"
    
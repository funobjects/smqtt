name := "smqtt"

organization := "org.funobjects"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("public")

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= {
  object v {
    val scalatest   = "2.2.4"
    val scalactic   = "2.2.4"
    val scalacheck  = "1.12.2"
    val scalaXml    = "1.0.3"
    val scodec      = "1.8.2"
  }
  Seq(
    "org.scodec"              %% "scodec-core"                        % v.scodec      withSources(),
    "org.scalactic"           %% "scalactic"                          % v.scalactic   withSources(),
    "org.scalacheck"          %% "scalacheck"                         % v.scalacheck  % "test",
    "org.scalatest"           %% "scalatest"                          % v.scalatest   % "test" withSources()
  )
}

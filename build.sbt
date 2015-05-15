name := "smqtt"

organization := "org.funobjects"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

resolvers += Resolver.sonatypeRepo("public")

resolvers ++= List(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Eclipse Paho Repository" at "https://repo.eclipse.org/content/repositories/paho-releases/")

libraryDependencies ++= {
  object v {
    val akka        = "2.3.11"
    val paho        = "1.0.2"
    val scalatest   = "2.2.4"
    val scalactic   = "2.2.4"
    val scalacheck  = "1.12.2"
    val scalaXml    = "1.0.3"
    val scodec      = "1.7.1"
  }
  Seq(
    "com.typesafe.akka"       %% "akka-persistence-experimental"      % v.akka        withSources(),
    "org.scodec"              %% "scodec-core"                        % v.scodec      withSources(),
    "org.scalactic"           %% "scalactic"                          % v.scalactic   withSources(),
    "com.typesafe.akka"       %% "akka-testkit"                       % v.akka        % "test" withSources(),
    "org.eclipse.paho"        %  "org.eclipse.paho.client.mqttv3"     % v.paho        % "test",
    "org.scalacheck"          %% "scalacheck"                         % v.scalacheck  % "test",
    "org.scalatest"           %% "scalatest"                          % v.scalatest   % "test" withSources()
  )
}

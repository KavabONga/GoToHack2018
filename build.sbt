name := "GoToHack2018"

version := "0.1"

scalaVersion := "2.11.1"

libraryDependencies ++= Seq (
  "com.typesafe.akka" % "akka-actor_2.11" % "2.3.4",
  "com.typesafe.akka" %% "akka-stream" % "2.5.21",
  "com.typesafe.akka" %% "akka-http"   % "10.1.7",
  "com.lightbend.akka" %% "akka-stream-alpakka-slick" % "1.0-M2",
  "com.typesafe.slick" %% "slick" % "3.0.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "org.json4s" %% "json4s-native" % "3.3.0"
)
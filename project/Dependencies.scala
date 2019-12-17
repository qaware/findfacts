import sbt._

object Dependencies {
  val circeVersion = "0.12.0"
  val playVersion = "2.7.3"

  val logging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
  val loggingBackend = Seq(
    "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.12.1"
      exclude ("org.slf4j", "slf4j-api"),
    "org.apache.logging.log4j" % "log4j-core" % "2.12.1"
  )
  val enum = "com.beachape" %% "enumeratum" % "1.5.13"
  val files = "com.github.pathikrit" %% "better-files" % "3.8.0"
  // Wire is only needed at compile-time
  val wire = "com.softwaremill.macwire" %% "macros" % "2.3.3" % "provided"
  val scalaTestBase = "org.scalatest" %% "scalatest" % "3.0.8"
  val scalaTest = scalaTestBase
  val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"
  val cmdOpts = "com.github.scopt" %% "scopt" % "3.7.1"
  val circeCore = "io.circe" %% "circe-core" % circeVersion
  val circeGeneric = "io.circe" %% "circe-generic" % circeVersion
  val circe = Seq(
    circeCore,
    circeGeneric,
    "io.circe" %% "circe-parser" % circeVersion
  )
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % "2.12.10"
  val swaggerUi = "org.webjars" % "swagger-ui" % "2.2.10"
  val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  val fastParse = "com.lihaoyi" %% "fastparse" % "2.1.3"
  val playJson = "com.typesafe.play" %% "play-json" % playVersion
  val solr = ("org.apache.solr" % "solr-core" % "8.2.0"
    exclude ("org.slf4j", "slf4j-api")
    exclude ("org.apache.logging.log4j", "log4j-api")
    exclude ("org.apache.logging.log4j", "log4j-core")
    exclude ("org.apache.logging.log4j", "log4j-slf4j-impl"))
  val playGuice = ("com.typesafe.play" %% "play-guice" % playVersion
    exclude ("org.slf4j", "slf4j-api"))
  val playCirce = "com.dripower" %% "play-circe" % "2712.0"
  val playTestPlus = ("org.scalatestplus.play" %% "scalatestplus-play" % "4.0.3"
    exclude ("org.slf4j", "slf4j-api"))
  val playSwagger = ("io.swagger" %% "swagger-play2" % "1.7.1"
    exclude ("com.google.guava", "guava")
    exclude ("com.typesafe.play", "play-logback_2.12"))
}

import sbt._

object Dependencies {
  val SCALA_MAJOR_VERSION = "2.13"
  val SCALA_VERSION = s"$SCALA_MAJOR_VERSION.4"

  val SOLR_VERSION = "8.2.0"
  val PLAY_VERSION = "2.8.7"

  val CIRCE_VERSION = "0.12.0"
  val PLAY_CIRCE_VERSION = "2812.0"
  val SWAGGER_ANNOTATIONS_VERSION = "1.5.22"
  val PLAY_SWAGGER_GEN_VERSION = "3.1.0"

  val LOG4J_VERSION = "2.12.1"
  val SCALATEST_VERSION = "3.2.5"
  val SCALAMOCK_VERSION = "1.16.29"
  val SCALALOGGING_VERSION = "3.9.2"

  val ENUMERATUM_VERSION = "1.6.1"
  val BETTER_FILES_VERSION = "3.9.1"
  val WIRE_VERSION = "2.3.7"
  val SCALATEST_PLUS_VERSION = "5.1.0"
  val SHAPELESS_VERSION = "2.3.3"
  val CATS_VERSION = "2.2.0"
  val SCOPTS_VERSION = "4.0.0"
  val PARSER_COMBINATORS_VERSION = "1.1.2"
  val FASTPARSE_VERSION = "2.3.1"
  val TUKAANI_VERSION = "1.8"
  val CLASSGRAPH_VERSION = "4.8.102"

  val SCAPEGOAT_VERSION = "1.3.11"

  val logging = "com.typesafe.scala-logging" %% "scala-logging" % SCALALOGGING_VERSION
  val loggingBackend = Seq(
    "org.apache.logging.log4j" % "log4j-slf4j-impl" % LOG4J_VERSION
      excludeAll ("org.slf4j" %% "slf4j-api"),
    "org.apache.logging.log4j" % "log4j-core" % LOG4J_VERSION
  )
  val enum = "com.beachape" %% "enumeratum" % ENUMERATUM_VERSION
  val files = "com.github.pathikrit" %% "better-files" % BETTER_FILES_VERSION
  // Wire is only needed at compile-time
  val wire = "com.softwaremill.macwire" %% "macros" % WIRE_VERSION % "provided"
  val scalaTest = "org.scalatest" %% "scalatest" % SCALATEST_VERSION
  val mockito =
    Seq(
      "org.mockito" %% "mockito-scala" % SCALAMOCK_VERSION,
      "org.mockito" %% "mockito-scala-scalatest" % SCALAMOCK_VERSION)
  val scalaTests = (scalaTest +: mockito).map(_ % "test")
  val shapeless = "com.chuusai" %% "shapeless" % SHAPELESS_VERSION
  val cats = "org.typelevel" %% "cats-core" % CATS_VERSION
  val cmdOpts = "com.github.scopt" %% "scopt" % SCOPTS_VERSION
  val circeCore = "io.circe" %% "circe-core" % CIRCE_VERSION
  val circeGeneric = "io.circe" %% "circe-generic" % CIRCE_VERSION
  val circe = Seq(
    circeCore,
    circeGeneric,
    "io.circe" %% "circe-parser" % CIRCE_VERSION
  )
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % SCALA_VERSION
  val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % PARSER_COMBINATORS_VERSION
  val fastParse = "com.lihaoyi" %% "fastparse" % FASTPARSE_VERSION
  val solr = ("org.apache.solr" % "solr-core" % SOLR_VERSION
    excludeAll ("org.slf4j" %% "slf4j-api")
    excludeAll "org.apache.logging.log4j")
  val playGuice = ("com.typesafe.play" %% "play-guice" % PLAY_VERSION
    excludeAll ("org.slf4j" %% "slf4j-api"))
  val playCirce = "com.dripower" %% "play-circe" % PLAY_CIRCE_VERSION
  val playTestPlus = ("org.scalatestplus.play" %% "scalatestplus-play" % SCALATEST_PLUS_VERSION
    excludeAll ("org.slf4j" %% "slf4j-api"))
  val swaggerAnnotations = "io.swagger" % "swagger-annotations" % SWAGGER_ANNOTATIONS_VERSION
  // Fork until io.swagger updates
  val playSwaggerGen = Seq(
    swaggerAnnotations,
    "com.github.dwickern" %% "swagger-play2.8" % PLAY_SWAGGER_GEN_VERSION
      excludeAll ("io.swagger" %% "swagger-annotations")
      excludeAll ("com.google.guava" %% "guava")
      excludeAll ("com.typesafe.play" %% "play-logback")
  )
  val classgraph = "io.github.classgraph" % "classgraph" % CLASSGRAPH_VERSION
  val isabelleDependencies = Seq("org.tukaani" % "xz" % TUKAANI_VERSION)
}

// Project-wide settings
ThisBuild / organization := "de.qaware.findfacts"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.10"
// Parallel execution causes logging issues
ThisBuild / Test / parallelExecution := false
// Enable compiler optimizations
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
)
// Missing resolvers for restlet stuff
ThisBuild / resolvers ++= Seq(
  "Restlet" at "https://maven.restlet.com/",
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  Resolver.mavenLocal
)
// Settings for sonarqube integration
ThisBuild / scapegoatVersion := "1.3.8"
ThisBuild / scapegoatIgnoredFiles += ".*scala.Using.scala"
ThisBuild / coverageExcludedFiles := "*.*scala.Using.scala"
def sonarSettings = Seq(
  sonarProperties ++= Map(
    "sonar.projectName" -> ("Isabelle AFP Search - " + name.value),
    "sonar.projectKey" -> ("de.qaware.isabelle-afp-search:" + name.value),
    "sonar.sources" -> (baseDirectory.value / "src" / "main" / "scala").getPath,
    "sonar.tests" -> (baseDirectory.value / "src" / "test" / "scala").getPath,
    "sonar.junit.reportPaths" -> (baseDirectory.value / "target" / "test-reports").getPath,
    "sonar.scala.coverage.reportPaths" -> (baseDirectory.value / "target" / "scala-2.12" / "scoverage-report" / "scoverage.xml").getPath,
    "sonar.scala.scapegoat.reportPaths" -> (baseDirectory.value / "target" / "scala-2.12" / "scapegoat-report" / "scapegoat-scalastyle.xml").getPath,
    "sonar.scala.scalastyle.reportPaths" -> (baseDirectory.value / "target" / "scalastyle-result.xml").getPath
  ))
val Sonar = Tags.Tag("sonar")
Global / concurrentRestrictions ++= Seq(
  Tags.limit(Sonar,  1),
  Tags.exclusive(Sonar),
  Tags.limitAll( 1 )
)
def sonar = Def.task { sonarScan.value } tag Sonar

// Project-wide dependencies (intersection from all modules that can be run on their own)
val scalatest = "org.scalatest" %% "scalatest" % "3.0.8"
ThisBuild / libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "com.beachape" %% "enumeratum" % "1.5.13",
  "com.github.pathikrit" %% "better-files" % "3.8.0",
  scalatest % "test")
// Additional named dependencies that are re-used multiple times
val wire = "com.softwaremill.macwire" %% "macros" % "2.3.3" % "provided"
val loggingBackend = Seq(
  "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.12.1"
  exclude ("org.slf4j", "slf4j-api"),
  "org.apache.logging.log4j" % "log4j-core" % "2.12.1"
)
val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"
val cmdOpts = "com.github.scopt" %% "scopt" % "3.7.1"
val circeVersion = "0.12.0"
val circeCore = "io.circe" %% "circe-core" % circeVersion
val circeGeneric = "io.circe" %% "circe-generic" % circeVersion
val circe = Seq(
  circeCore,
  circeGeneric,
  "io.circe" %% "circe-parser" % circeVersion
)

// Profiles
val UiProfile = "ui"
val LoaderProfile = "loader"
val Profiles = Set(UiProfile, LoaderProfile)
lazy val profiles = {
  val selected: Seq[String] = Option(System.getProperty("profiles")).toSeq.flatMap(_.split(","))
  selected.filterNot(Profiles.contains).map(p => throw new IllegalArgumentException(s"Profile $p does not exist!"))
  selected
}

// Root project aggregates all
lazy val root = (project in file("."))
  // Aggregate all modules
  .aggregate(core, ui, loaders, `yxml-parser`, `common-dt`, `common-utils`, `common-solr`)
  .disablePlugins(SonarPlugin)

// Controls aggregation of subproject that need the elm-compiler (depending if ui profile is active)
lazy val ui = project
  .settings(aggregate := profiles.contains(UiProfile))
  .aggregate(`webapp`)
  .disablePlugins(SonarPlugin)

// Controls aggregation of subprojects that need isablelle (depending if loaders profile is active)
lazy val loaders = project
  .settings(aggregate := profiles.contains(LoaderProfile))
  .aggregate(`solr-dump-importer`)
  .disablePlugins(SonarPlugin)

// Real sub-projects
// Importer for isabelle dumps
lazy val `solr-dump-importer` = project
  .configs(IntegrationTest)
  .settings(
    sonarSettings,
    fork in run := true,
    javaOptions ++= Seq("-Xmx24G", "-Xss512m"),
    Defaults.itSettings,
    libraryDependencies ++= loggingBackend ++ Seq(
      cmdOpts,
      "org.scala-lang" % "scala-compiler" % "2.12.10" % "it",
      scalatest % "it"
    ),
  )
  .dependsOn(`common-solr`, `common-dt`, `yxml-parser`, `common-utils`, `isabelle`)

// Isabelle project dependency
lazy val isabelle = project
  .settings(unmanagedJars in Compile ++= (baseDirectory.value / "lib" / "classes" ** "*.jar").get())
  .disablePlugins(SonarPlugin)

// Full-stack play web application, with elm ui
lazy val `webapp` = project
  .settings(
    sonarSettings,
    sonarProperties ++= Map(
      "sonar.sources" -> (baseDirectory.value / "app").getPath,
      "sonar.tests" -> (baseDirectory.value / "test").getPath,
    ),
    // Resource loading doesn't work properly in 'run' mode (only in prod), so we need to specify the logging conf here
    javaOptions in Runtime += "-Dlog4j.configurationFile=" + (file("webapp") / "conf" / "log4j2.properties").getPath,
    libraryDependencies ++= (loggingBackend ++ circe ++ Seq(
      wire,
      guice exclude ("org.slf4j", "slf4j-api"),
      "com.dripower" %% "play-circe" % "2712.0",
      "org.scalatestplus.play" %% "scalatestplus-play" % "4.0.3" % Test exclude ("org.slf4j", "slf4j-api"),
      "io.swagger" %% "swagger-play2" % "1.7.1"
        exclude("com.google.guava", "guava")
        exclude("com.typesafe.play", "play-logback_2.12"),
      "org.webjars" % "swagger-ui" % "2.2.10"
    )),
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLogback, SbtElm)
  .dependsOn(core, `webapp-ui`)

lazy val `webapp-ui` = project
  .settings(
    // Add elm sources to assets
    unmanagedSourceDirectories in Assets += baseDirectory.value / "src" / "elm",
  )
  .enablePlugins(SbtWeb)
  .disablePlugins(SonarPlugin)


// Parser for yxml
lazy val `yxml-parser` = project
  .settings(
    sonarSettings,
    libraryDependencies ++= loggingBackend ++ Seq(
    cmdOpts,
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "com.lihaoyi" %% "fastparse" % "2.1.3"
  ))

lazy val core = project
  .configs(IntegrationTest)
  .settings(
    sonarSettings,
    Defaults.itSettings,
    libraryDependencies ++= loggingBackend.map(_ % "it") ++ Seq(
      wire,
      shapeless,
      circeGeneric,
      scalatest % "it,test"
    )
  )
  .dependsOn(`common-dt`, `common-solr`, `common-utils`)

// Common data types
lazy val `common-dt` = project
  .settings(
    sonarSettings,
    libraryDependencies ++= Seq(
      shapeless,
      circeCore,
      "com.typesafe.play" %% "play-json" % "2.7.4"
    )
  )
  .dependsOn(`common-utils`)

// Common utility
lazy val `common-utils` = project
  .settings(sonarSettings)

// Common solr entities and data access
lazy val `common-solr` = project
  .settings(
    sonarSettings,
    libraryDependencies += "org.apache.solr" % "solr-core" % "8.2.0"
      exclude ("org.slf4j", "slf4j-api")
      exclude ("org.apache.logging.log4j", "log4j-api")
      exclude ("org.apache.logging.log4j", "log4j-core")
      exclude ("org.apache.logging.log4j", "log4j-slf4j-impl")
  )
  .dependsOn(`common-dt`, `common-utils`)

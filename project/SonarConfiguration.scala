import com.sksamuel.scapegoat.sbt.ScapegoatSbtPlugin
import com.sksamuel.scapegoat.sbt.ScapegoatSbtPlugin.autoImport._
import sbt.Def.Setting
import sbt.Keys.{aggregate, crossTarget, name, sourceDirectory}
import sbt.io.syntax._
import sbt.{AutoPlugin, Compile, Def, IntegrationTest, Plugins, ProjectReference, SettingKey, Test}
import sbtsonar.SonarPlugin
import sbtsonar.SonarPlugin.autoImport._

/** Singleton-plugin for correct sonar configuration. */
object SonarConfiguration extends AutoPlugin {
  override def requires: Plugins = SonarPlugin && ScapegoatSbtPlugin

  /** Auto-import object so settings are found automatically by sbt. */
  object autoImport {
    lazy val sonarProjects: SettingKey[Seq[ProjectReference]] =
      SettingKey[Seq[ProjectReference]]("all sonar subprojects")
  }

  import autoImport._

  /** Retrieves sonarProjects' names, in a comma-seperated string. */
  def sonarModules: Def.Initialize[String] =
    Def.settingDyn {
      val names = sonarProjects.value.map(project => Def.setting(name.in(project).value))
      Def.Initialize.joinInitialize(names).join.apply(_.mkString(","))
    }

  /** Retrieves key-value-pairs of sonar settings. */
  def sonarModuleSettings: Def.Initialize[Map[String, String]] =
    Def.settingDyn {
      val settings = sonarProjects.value map { project =>
        Def.setting {
          // Read out paths, as the plugin doesn't to that correctly
          val projectName = name.in(project).value
          val sourceDir = (project / Compile / sourceDirectory).value.getPath
          val testSources =
            Seq((project / Test / sourceDirectory).value, (project / IntegrationTest / sourceDirectory).value)
              .filter(_.exists)
              .map(_.getPath)
              .filterNot(sourceDir.contains(_))
              .mkString(",")

          val crossTargetDir = (project / Compile / crossTarget).value
          // Build sonar config map
          Seq(
            s"$projectName.sonar.sources" -> sourceDir,
            s"$projectName.sonar.tests" -> testSources,
            s"$projectName.sonar.junit.reportPaths" -> (crossTargetDir / ".." / "test-reports").getPath,
            s"$projectName.sonar.scala.coverage.reportPaths" ->
              (crossTargetDir / "scoverage-report" / "scoverage.xml").getPath,
            s"$projectName.sonar.scala.scapegoat.reportPaths" ->
              (crossTargetDir / "scapegoat-report" / "scapegoat-scalastyle.xml").getPath
          )
        }
      }
      Def.Initialize.join(settings).apply(_.flatten.toMap)
    }

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    sonarProperties ++= Map(
      "sonar.projectName" -> "Isabelle AFP Search",
      "sonar.projectKey" -> "de.qaware.isabelle-afp-search:root",
      "sonar.exclusions" -> "**/scala/Using*",
      "sonar.modules" -> sonarModules.value
    ) ++ sonarModuleSettings.value,
    aggregate in sonarScan := false
  )

  override def buildSettings: Seq[Def.Setting[_]] =
    Seq(
      scapegoatVersion := Dependencies.SCAPEGOAT_VERSION
    )
}

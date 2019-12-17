import com.sksamuel.scapegoat.sbt.ScapegoatSbtPlugin
import com.sksamuel.scapegoat.sbt.ScapegoatSbtPlugin.autoImport._
import sbt.Def.Setting
import sbt.Keys.{aggregate, name, sourceDirectory, target}
import sbt.io.syntax._
import sbt.{AutoPlugin, Compile, Def, Plugins, ProjectReference, SettingKey, Test}
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
  def sonarModules: Def.Initialize[String] = Def.settingDyn {
    val names = sonarProjects.value.map(project => Def.setting(name.in(project).value))
    Def.Initialize.joinInitialize(names).join.apply(_.mkString(","))
  }

  /** Retrieves key-value-pairs of sonar settings. */
  def sonarModuleSettings: Def.Initialize[Map[String, String]] = Def.settingDyn {
    val settings = sonarProjects.value map { project =>
      Def.setting {
        // Read out paths, as the plugin doesn't to that correctly
        val projectName = name.in(project).value
        val sourceDir = (project / Compile / sourceDirectory).value.getPath
        val testDir = (project / Test / sourceDirectory).value.getPath
        val targetDir = (project / Compile / target).value
        // Build sonar config map
        Seq(
          (s"$projectName.sonar.sources", sourceDir),
          (s"$projectName.sonar.tests" -> testDir),
          (s"$projectName.sonar.junit.reportPaths" -> (targetDir / "test-reports").getPath),
          (s"$projectName.sonar.scala.coverage.reportPaths" ->
            (targetDir / "scala-2.12" / "scoverage-report" / "scoverage.xml").getPath),
          (s"$projectName.sonar.scala.scapegoat.reportPaths" ->
            (targetDir / "scala-2.12" / "scapegoat-report" / "scapegoat-scalastyle.xml").getPath),
          (s"$projectName.sonar.scala.scalastyle.reportPaths" -> (targetDir / "scalastyle-result.xml").getPath)
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
      "sonar.modules" -> sonarModules.value) ++ sonarModuleSettings.value,
    aggregate in sonarScan := false
  )

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    scapegoatVersion := "1.3.8"
  )
}

package de.qaware.findfacts.dumpimporter

import java.net.URL

import better.files.File
import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.solr.{CloudSolr, LocalSolr, RemoteSolr, SolrRepository, ZKHost}
import de.qaware.findfacts.dumpimporter.steps.pide.LoadPideMarkupStep
import de.qaware.findfacts.dumpimporter.steps.thyexport.LoadThyExportStep
import de.qaware.findfacts.dumpimporter.steps.{SanityCheckStep, StepContext, TranslateNameStep, WriteSolrStep}
import scopt.{OptionParser, Read}

/** Intermediate data to build the config.
  *
  * @param dump specified isabelle dump directories
  * @param solr specified solr repositories to write to
  */
final case class ConfigBuilder(dump: Seq[File] = Seq.empty, solr: Seq[SolrRepository] = Seq.empty) {

  /** Builds a config from this builder.
    *
    * @return built config
    */
  @SuppressWarnings(Array("TraversableHead")) // Justification: checked in parser
  def buildConfig: Config = Config(dump.head, solr.head)
}

/** Configuration of the importer.
  *
  * @param dump isabelle dump directory
  * @param solr solr repository to write to
  */
final case class Config(dump: File = File(""), solr: SolrRepository)

/** Command-line interface app for importer. */
object ImporterApp extends App {

  /** Parsing of zookeeper hosts, specified as 'host:port' strings */
  implicit val zkHostRead: Read[ZKHost] = Read.reads(zkhost =>
    zkhost.indexOf(':') match {
      case -1 => throw new IllegalArgumentException("Expected host:port")
      case n: Any => ZKHost(zkhost.take(n), Integer.parseInt(zkhost.drop(n + 1)))
  })

  /** Parse files from better.files */
  implicit val betterFileRead: Read[File] = Read.reads(File(_))

  private val builder = new OptionParser[ConfigBuilder]("solr-dump-importer") {
    head("Imports theory data from isabelle dump into solr")
    arg[File]("<dump>")
      .required()
      .action((file, conf) => conf.copy(dump = conf.dump :+ file))
      .text("isabelle dump dir")
    opt[File]('l', "local-solr")
      .valueName("<dir>")
      .text("local solr home directory")
      .action((dir, conf) => conf.copy(solr = conf.solr :+ LocalSolr(dir)))
    opt[URL]('e', "external-solr")
      .valueName("<url>")
      .text("external solr host")
      .action((url, conf) => conf.copy(solr = conf.solr :+ RemoteSolr(url)))
    opt[Seq[ZKHost]]('c', "cloud-solr")
      .valueName("<zk1>,<zk2>...")
      .text("solr cloud zookeeper hosts")
      .action((hosts, conf) => conf.copy(solr = conf.solr :+ CloudSolr(hosts)))
    checkConfig {
      case ConfigBuilder(Seq(_), Seq(_)) => success
      case _ => failure("Must specify exactly one connection")
    }
  }

  private val logger = Logger[ImporterApp.type]

  // scalastyle:off
  builder.parse(args, ConfigBuilder()) match {
    case Some(confBuilder) =>
      val config = confBuilder.buildConfig
      logger.info("Starting import...")
      if (config.solr.getClass != classOf[LocalSolr]) {
        // Check if non-embedded solr instance is available to fail early if it is not
        config.solr.solrConnection().close()
        logger.info("Could successfully connect to solr at {}", config.solr)
      }
      val context = StepContext.empty
      val steps = Seq(
        new LoadThyExportStep(config),
        new TranslateNameStep(config),
        new LoadPideMarkupStep(config),
        new SanityCheckStep(config),
        new WriteSolrStep(config)
      )
      steps.zipWithIndex foreach {
        case (step, i) =>
          logger.info("Step {}/{}...", i + 1, steps.size)
          step.apply(context)
      }
      logger.info("Finished importing.")
    case _ =>
  }
}

package de.qaware.dumpimporter

import java.io.File
import java.net.URL

import scala.util.Using

import de.qaware.common.solr.{CloudSolr, LocalSolr, RemoteSolr, SolrRepository, ZKHost}
import scopt.{OptionParser, Read}

/** Configuration of the importer.
  *
  * @param dump isabelle dump directory
  * @param solr solr repository to write to
  * @param isValid help flag for argument parsing
  */
sealed case class Config(dump: File = new File("."), solr: SolrRepository = new LocalSolr(), isValid: Boolean = true)

/** Command-line interface app for importer. */
object ImporterApp extends App {

  /**
    * Parsing of zookeeper hosts, specified as 'host:port' strings
    */
  implicit val zkHostRead: Read[ZKHost] = Read.reads(zkhost =>
    zkhost.indexOf(':') match {
      case -1     => throw new IllegalArgumentException("Expected host:port")
      case n: Any => ZKHost(zkhost.take(n), Integer.parseInt(zkhost.drop(n + 1)))
  })

  private val builder = new OptionParser[Config]("solr dump importer") {
    head("Imports theory data from isabelle dump into solr")
    arg[File]("<dump>")
      .required()
      .action((file, conf) => conf.copy(dump = file))
      .text("isabelle dump dir")
    opt[URL]('e', "external-solr")
      .valueName("<url>")
      .action((url, conf) =>
        conf.solr match {
          case _: LocalSolr => conf.copy(solr = RemoteSolr(url))
          case _            => conf.copy(isValid = false)
      })
      .text("external solr host")
    opt[Seq[ZKHost]]('c', "cloud-solr")
      .valueName("<zk1>,<zk2>...")
      .action((hosts, conf) =>
        conf.solr match {
          case _: LocalSolr => conf.copy(solr = CloudSolr(hosts))
          case _            => conf.copy(isValid = false)
      })
      .text("solr cloud zookeeper hosts")
    checkConfig(c => if (c.isValid) success else failure("Only one solr connection may be specified!"))
  }

  builder.parse(args, Config()) match {
    case Some(config) =>
      Using(config.solr.getSolrConnection) { client =>
        // TODO
      }
    case _ =>
  }
}

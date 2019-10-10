package de.qaware.dumpimporter

import java.io.File
import java.net.URL
import java.util.UUID

import scala.util.Using

import com.typesafe.scalalogging.Logger
import de.qaware.common.solr.{CloudSolr, LocalSolr, RemoteSolr, SolrRepository, ZKHost}
import de.qaware.common.solr.dt.ConstEntity
import org.apache.solr.client.solrj.SolrQuery
import scopt.{OptionParser, Read}

/** Configuration of the importer.
  *
  * @param dump isabelle dump directory
  * @param solr solr repository to write to
  * @param isValid help flag for argument parsing
  */
sealed case class Config(dump: File = new File("."), solr: Option[SolrRepository] = None, isValid: Boolean = true)

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

  val addRepo: (Config, SolrRepository) => Config = (conf, repo) =>
    if (conf.solr.isEmpty) conf.copy(solr = Some(repo)) else conf.copy(isValid = false)

  private val builder = new OptionParser[Config]("solr-dump-importer") {
    head("Imports theory data from isabelle dump into solr")
    arg[File]("<dump>")
      .required()
      .action((file, conf) => conf.copy(dump = file))
      .text("isabelle dump dir")
    opt[File]('l', "local-solr")
      .valueName("<dir>")
      .text("local solr home directory")
      .action((dir, conf) => addRepo(conf, LocalSolr(dir)))
    opt[URL]('e', "external-solr")
      .valueName("<url>")
      .text("external solr host")
      .action((url, conf) => addRepo(conf, RemoteSolr(url)))
    opt[Seq[ZKHost]]('c', "cloud-solr")
      .valueName("<zk1>,<zk2>...")
      .text("solr cloud zookeeper hosts")
      .action((hosts, conf) => addRepo(conf, CloudSolr(hosts)))
    checkConfig(c =>
      if (c.solr.isEmpty) {
        failure("Must specify a solr connection")
      } else if (!c.isValid) {
        failure("Only one solr connection may be specified!")
      } else {
        success
    })
  }

  private val logger = Logger("ImporterApp")

  builder.parse(args, Config()) match {
    case Some(config) =>
      logger.info("Read config: {}", config)
      Using.resource(config.solr.get.getSolrConnection) { client =>
        logger.info("Connected to solr at {}", config.solr)
        // TODO
        val bean =
          ConstEntity(UUID.randomUUID().toString, "testsrc", 1, 2, "testname", "'a=>'b", "def", Array("1", "2"))
        client.addBean(bean)
        client.commit()
        val beans = client.query(new SolrQuery("*:*")).getBeans(classOf[ConstEntity])
        logger.info("Found {} beans. First: {}", beans.size, beans.get(0))
      }
      logger.info("Finished importing.")
    case _ =>
  }
}

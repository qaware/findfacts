package de.qaware.common.solr

import java.net.URL
import java.nio.file.{Files, StandardCopyOption}

import scala.collection.JavaConverters._

import better.files.{File, Resource}
import com.typesafe.scalalogging.Logger
import de.qaware.scalautils.Using
import org.apache.solr.client.solrj.SolrClient
import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.apache.solr.client.solrj.impl.{CloudSolrClient, HttpSolrClient}

/** Repository to provide connections to different types of solr instances. */
sealed trait SolrRepository {

  /** Creates a configured and initialized client to the repositories' solr instance.
    *
    * @return a configured, initialized client to a solr instance
    */
  def solrConnection: SolrClient
}

/** Local, embedded solr client.
  *
  * @param solrHome sole home directory to read config from/write data to
  */
case class LocalSolr(solrHome: File) extends SolrRepository {

  /** Name of the default core for embedded solr. */
  final val CORE_NAME = "theorydata"
  /** Solr config files that are created per default. */
  final val SOLR_CONF_FILES = Seq(
    "solr.xml",
    "theorydata/schema.xml",
    "theorydata/enumsConfig.xml",
    "theorydata/core.properties",
    "theorydata/conf/solrconfig.xml"
  )

  private val logger = Logger[LocalSolr]

  require(solrHome.isDirectory, "Solr home does not exist")
  require(solrHome.isWriteable, "No write access to solr home directory")

  private lazy val client = {
    logger.info("Starting up embedded solr server...")
    // Unpack solr resources
    SOLR_CONF_FILES.map(res =>
      Using.resource(Resource.getAsStream("solr/" + res)) { stream =>
        val file = solrHome.canonicalFile / res

        if (stream == null) {
          throw new IllegalStateException("Could not find config file " + file)
        }

        logger.info("Writing solr config file {}", file)
        file.createDirectoryIfNotExists()
        Files.copy(stream, file.path, StandardCopyOption.REPLACE_EXISTING)
    })

    new EmbeddedSolrServer(solrHome.path, CORE_NAME)
  }

  override def solrConnection: SolrClient = client
}

/** Remote http solr client.
  *
  * @param url to solr instance
  */
case class RemoteSolr(url: URL) extends SolrRepository {
  override def solrConnection: SolrClient = new HttpSolrClient.Builder().withBaseSolrUrl(url.toString).build
}

/** Remote solr cloud.
  *
  * @param zkhosts list of zookeeper hosts
  */
case class CloudSolr(zkhosts: Seq[ZKHost]) extends SolrRepository {
  require(zkhosts.nonEmpty, "must have at least one zookeeper")

  override def solrConnection: SolrClient = {
    val hosts: java.util.List[String] = zkhosts.map(zkhost => zkhost.host + zkhost.port.toString).toBuffer.asJava
    new CloudSolrClient.Builder(hosts).build
  }
}

/** Container for zookeeper host identifier, conststing of host address and port.
  *
  * @param host zookeeper host address
  * @param port zookeeper port
  */
case class ZKHost(host: String, port: Int) {
  require(host.nonEmpty, "host must not be empty")
  require(port > 0 && port < 65535, "port must be between 0 and 65535, was " + port)
}

package de.qaware.findfacts.common.solr

import java.net.URL
import java.nio.file.{Files, StandardCopyOption}

import better.files.{File, Resource}
import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.scala.Using
import org.apache.solr.client.solrj.SolrClient
import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.apache.solr.client.solrj.impl.{CloudSolrClient, HttpSolrClient}

import scala.collection.JavaConverters._

/** Repository to provide connections to different types of solr instances. */
sealed trait SolrRepository {

  /** Creates a configured and initialized client to the repositories' solr instance.
    *
    * @return a configured, initialized client to a solr instance
    */
  def solrConnection(): SolrClient
}

/** Local, embedded solr client.
  *
  * @param solrHome sole home directory to read config from/write data to
  */
final case class LocalSolr(solrHome: File) extends SolrRepository {

  /** Name of the default core for embedded solr. */
  final val EmbeddedCoreName = "theorydata"

  /** Solr config files that are created per default. */
  final val SolrConfFiles = Seq(
    "solr.xml",
    s"$EmbeddedCoreName/schema.xml",
    s"$EmbeddedCoreName/enumsconfig.xml",
    s"$EmbeddedCoreName/core.properties",
    s"$EmbeddedCoreName/conf/solrconfig.xml"
  )

  private val logger = Logger[LocalSolr]

  require(solrHome.isDirectory, s"Solr home $solrHome does not exist")
  require(solrHome.isWriteable, s"No write access to solr home directory $solrHome")

  override lazy val solrConnection: SolrClient = {
    logger.info("Starting up embedded solr server...")
    // Unpack solr resources
    SolrConfFiles.map(res =>
      Using.resource(Resource.getAsStream("solr/" + res)) { stream =>
        val file = solrHome.canonicalFile / res

        if (stream == null) {
          throw new IllegalStateException("Could not find config file " + file)
        }

        logger.info("Writing solr config file {}", file)
        file.createDirectoryIfNotExists()
        Files.copy(stream, file.path, StandardCopyOption.REPLACE_EXISTING)
    })

    new EmbeddedSolrServer(solrHome.path, EmbeddedCoreName)
  }
}

/** Remote http solr client.
  *
  * @param url to solr instance
  */
final case class RemoteSolr(url: URL) extends SolrRepository {
  override lazy val solrConnection: SolrClient = new HttpSolrClient.Builder().withBaseSolrUrl(url.toString).build
}

/** Remote solr cloud.
  *
  * @param zkhosts list of zookeeper hosts
  */
final case class CloudSolr(zkhosts: Seq[ZKHost]) extends SolrRepository {
  require(zkhosts.nonEmpty, "must have at least one zookeeper")

  override lazy val solrConnection: SolrClient = {
    val hosts: java.util.List[String] = zkhosts.map(zkhost => zkhost.host + zkhost.port.toString).toBuffer.asJava
    new CloudSolrClient.Builder(hosts).build
  }
}

/** Container for zookeeper host identifier, conststing of host address and port.
  *
  * @param host zookeeper host address
  * @param port zookeeper port
  */
final case class ZKHost(host: String, port: Int) {
  require(host.nonEmpty, "host must not be empty")
  require(port > 0 && port < 65535, "port must be between 0 and 65535, was " + port)
}

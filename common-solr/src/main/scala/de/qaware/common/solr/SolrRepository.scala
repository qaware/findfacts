package de.qaware.common.solr

import java.io.File
import java.net.URL
import java.nio.file.{Files, Path, StandardCopyOption}

import scala.jdk.CollectionConverters._
import scala.util.Using

import com.typesafe.scalalogging.Logger
import org.apache.solr.client.solrj.SolrClient
import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.apache.solr.client.solrj.impl.{CloudSolrClient, HttpSolrClient}

/**
  * Repository to provide connections to different types of solr instances.
  */
sealed trait SolrRepository {

  /**
    * Creates a configured and initialized client to the repositories' solr instance.
    *
    * @return a configured, initialized client to a solr instance
    */
  def getSolrConnection: SolrClient
}

/**
  * Local, embedded solr client.
  */
case class LocalSolr(solrHome: File) extends SolrRepository {

  /** Name of the default core for embedded solr */
  final val CORE_NAME = "theorydata"
  final val SOLR_CONF_FILES = Seq(
    "solr.xml",
    "theorydata/schema.xml",
    "theorydata/enumsConfig.xml",
    "theorydata/core.properties",
    "theorydata/conf/solrconfig.xml"
  )

  private val logger = Logger[LocalSolr]

  require(solrHome.isDirectory, "Solr home does not exist")
  require(solrHome.canWrite, "No write access to solr home directory")

  private lazy val client = {
    logger.info("Starting up embedded solr server...")
    // Unpack solr resources
    SOLR_CONF_FILES.map(res =>
      Using.resource(getClass.getResourceAsStream("/solr/" + res)) { stream =>
        if (stream == null) {
          throw new IllegalStateException("Could not find config file " + res + " in " + solrHome.getCanonicalPath)
        }
        val destDir = Path.of(solrHome.getCanonicalPath, res).getParent.toFile
        if (!destDir.exists()) {
          logger.info("Creating config dir: {}", destDir)
          Files.createDirectory(destDir.toPath)
        }
        logger.info("Writing config file {} to {}", res, solrHome.getCanonicalPath)
        Files.copy(stream, Path.of(solrHome.getCanonicalPath, res), StandardCopyOption.REPLACE_EXISTING)
    })

    new EmbeddedSolrServer(solrHome.toPath, CORE_NAME)
  }

  override def getSolrConnection: SolrClient = client
}

/**
  * Remote http solr client.
  *
  * @param url to solr instance
  */
case class RemoteSolr(url: URL) extends SolrRepository {
  override def getSolrConnection: SolrClient = new HttpSolrClient.Builder().withBaseSolrUrl(url.toString).build
}

/**
  * Remote solr cloud.
  *
  * @param zkhosts list of zookeeper hosts
  */
case class CloudSolr(zkhosts: Seq[ZKHost]) extends SolrRepository {
  require(zkhosts.nonEmpty, "must have at least one zookeeper")

  override def getSolrConnection: SolrClient = {
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

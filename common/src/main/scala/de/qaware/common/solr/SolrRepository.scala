package de.qaware.common.solr

import java.io.File
import java.net.URL
import java.util

import org.apache.solr.client.solrj.SolrClient
import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.apache.solr.client.solrj.impl.{CloudSolrClient, HttpSolrClient}

import scala.jdk.CollectionConverters._

/**
  * Repository to provide connections to different types of solr instances.
  */
trait SolrRepository {

  /**
    * Creates a configured and initialized client to the repositories' solr instance.
    *
    * @return a configured, initialized client to a solr instance
    */
  def getSolrConnection: SolrClient
}

object LocalSolr {
  val CORE_NAME = "theorydata"
  val SOLR_HOME = "./solr" // TODO resource level
}

/**
  * Local, embedded solr client.
  */
case class LocalSolr() extends SolrRepository {
  override def getSolrConnection: SolrClient =
    new EmbeddedSolrServer(new File(LocalSolr.SOLR_HOME).toPath, LocalSolr.CORE_NAME)
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
  val hosts: util.List[String] = zkhosts.map(zkhost => zkhost.host + zkhost.port.toString).toBuffer.asJava
  override def getSolrConnection: SolrClient = new CloudSolrClient.Builder(hosts).build
}

case class ZKHost(host: String, port: Int) {
  require(host.nonEmpty, "host must not be empty")
  require(port > 0 && port < 65535, "port must be between 0 and 65535, was " + port)
}

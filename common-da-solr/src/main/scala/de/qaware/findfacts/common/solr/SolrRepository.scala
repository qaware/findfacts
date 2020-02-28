package de.qaware.findfacts.common.solr

import java.io.{File => JFile}
import java.net.URL
import java.nio.file.{Files, StandardCopyOption}
import java.util.UUID

import scala.collection.JavaConverters._

import better.files.{File, Resource}
import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.scala.Using
import org.apache.solr.client.solrj.SolrRequest.METHOD
import org.apache.solr.client.solrj.{SolrClient, SolrRequest, SolrResponse}
import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.apache.solr.client.solrj.impl.{CloudSolrClient, HttpSolrClient}
import org.apache.solr.common.util.NamedList

/** Repository to provide connections to different types of solr instances. */
sealed trait SolrRepository {

  /** Creates a configured and initialized client to the repositories' solr instance.
    *
    * @return a configured, freshly initialized client to a solr instance
    */
  def solrConnection(): SolrClient
}

/** Local, embedded solr client.
  *
  * @param solrHome sole home directory to read config from/write data to
  * @param core solr core name
  */
final class LocalSolr private (solrHome: File, core: String) extends SolrRepository {

  /** Resource dir in which solr config files are in. */
  final val SolrResourceDir = "solr"

  /** Solr config file. */
  final val SolrConfFile = "solr.xml"

  /** Core config files that are created per default. */
  final val SolrCoreFiles = Seq(
    "schema.xml",
    "enumsconfig.xml",
    "core.properties",
    "conf/solrconfig.xml"
  )

  require(Resource.url(s"$SolrResourceDir/$SolrConfFile").isDefined)
  require(SolrCoreFiles.forall(f => Resource.url(s"$SolrResourceDir/$core/$f").isDefined))

  private val logger = Logger[LocalSolr]

  require(solrHome.isDirectory, s"Solr home $solrHome does not exist")
  require(solrHome.isWriteable, s"No write access to solr home directory $solrHome")

  override def solrConnection(): SolrClient = {
    logger.info(s"Starting up embedded solr server...")
    // Unpack solr resources
    val srcDestToCopy = (s"$SolrResourceDir/$SolrConfFile" -> solrHome.canonicalFile / SolrConfFile) +:
      SolrCoreFiles.map(res => s"$SolrResourceDir/$core/$res" -> solrHome.canonicalFile / core / res)

    srcDestToCopy foreach {
      case (src, dest) =>
        Using.resource(Resource.getAsStream(src)) { stream =>
          if (stream == null) {
            throw new IllegalStateException("Could not find config file " + dest)
          }

          logger.info("Writing solr config file {}", dest)
          dest.createDirectoryIfNotExists()
          Files.copy(stream, dest.path, StandardCopyOption.REPLACE_EXISTING)
        }
    }

    val solrServer = new EmbeddedSolrServer(solrHome.path, core)
    // Workaround for https://issues.apache.org/jira/browse/SOLR-12858
    new SolrClient {
      override def request(request: SolrRequest[_ <: SolrResponse], collection: String): NamedList[AnyRef] = {
        val isContentStreamQuery = request.getParams == null || !request.getParams.getParameterNamesIterator.hasNext
        if (request.getMethod == METHOD.POST && !isContentStreamQuery) {
          request.setMethod(METHOD.GET)
        }
        solrServer.request(request, collection)
      }
      override def close(): Unit = solrServer.close()
    }
  }
}

/** Companion object. */
object LocalSolr {

  /** Creates new local solr repository.
    *
    * @param solrHome directory for solr instance to live in
    * @param core name of the core - config files have to be under resources/solr
    * @return local solr repository
    */
  def apply(solrHome: JFile, core: String): LocalSolr = new LocalSolr(File(solrHome.getCanonicalPath), core)
}

/** Remote http solr client.
  *
  * @param url to solr instance
  */
final class RemoteSolr(url: URL) extends SolrRepository {
  override def solrConnection(): SolrClient =
    new HttpSolrClient.Builder()
      .withBaseSolrUrl(url.toString)
      .withConnectionTimeout(5 * 60 * 1000)
      .withSocketTimeout(5 * 60 * 1000)
      .build
}

/** Companion object. */
object RemoteSolr {

  /** Creates a new remote solr repository.
    *
    * @param host address
    * @param port on which solr runs
    * @param core name
    * @return configured remote solr repository
    */
  def apply(host: String, port: Int, core: String): RemoteSolr =
    new RemoteSolr(new URL("http", host, port, s"/solr/$core"))
}

/** Remote solr cloud.
  *
  * @param zkhosts list of zookeeper hosts
  */
final case class CloudSolr(zkhosts: Seq[ZKHost]) extends SolrRepository {
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
final case class ZKHost(host: String, port: Int) {
  require(host.nonEmpty, "host must not be empty")
  require(port > 0 && port < 65535, "port must be between 0 and 65535, was " + port)
}

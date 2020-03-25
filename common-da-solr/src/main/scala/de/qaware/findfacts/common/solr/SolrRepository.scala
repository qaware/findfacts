package de.qaware.findfacts.common.solr

import java.io.{File => JFile}
import java.net.URL
import java.nio.file.{Files, StandardCopyOption}

import better.files.{File, Resource}
import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.scala.Using
import org.apache.solr.client.solrj.SolrRequest.METHOD
import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.apache.solr.client.solrj.impl.{CloudSolrClient, HttpSolrClient}
import org.apache.solr.client.solrj.request.{CollectionAdminRequest, CoreAdminRequest}
import org.apache.solr.client.solrj.{SolrClient, SolrRequest, SolrResponse}
import org.apache.solr.common.params.CoreAdminParams.CoreAdminAction
import org.apache.solr.common.util.NamedList

import scala.collection.JavaConverters._

/** Repository to provide connections to different types of solr instances. */
sealed trait SolrRepository extends SolrClient with AutoCloseable {

  /** Creates a solr index for the repository, if not present.
    *
    * @param name of the index to create.
    * @return true if a new index was created or false if it already existed
    */
  def createIndex(name: String): Boolean

  /** List available indexes.
    *
    * @return list of available indexes
    */
  def listIndexes: List[String]
}

/** Local, embedded solr client.
  *
  * @param server embedded underlying solr server
  * @param home solr home directory to read config from/write data to
  * @param solrResourceDir resource directory where solr configuration is found
  */
final class LocalSolr private (
    private val server: EmbeddedSolrServer,
    private val home: File,
    private val solrResourceDir: String)
    extends SolrRepository {

  private val logger = Logger[LocalSolr]

  override def request(request: SolrRequest[_ <: SolrResponse], collection: String): NamedList[AnyRef] = {
    // Workaround for https://issues.apache.org/jira/browse/SOLR-12858
    val isContentStreamQuery = request.getParams == null || !request.getParams.getParameterNamesIterator.hasNext
    if (request.getMethod == METHOD.POST && !isContentStreamQuery) {
      request.setMethod(METHOD.GET)
    }
    server.request(request, collection)
  }

  override def createIndex(name: String): Boolean = this.synchronized {
    val coreConfDir = home / name / LocalSolr.SolrConfDir
    if (coreConfDir.exists) {
      server.getCoreContainer.reload(name)
      false
    } else {
      // Prepare core
      coreConfDir.createDirectories

      val confResourceDir = s"$solrResourceDir/${LocalSolr.SolrConfDir}"
      val confFiles = Resource.getAsString(confResourceDir).linesIterator

      logger.info(s"Populating $coreConfDir with configuration from $confResourceDir...")
      confFiles.foreach { file =>
        Using.resource(Resource.getAsStream(s"$confResourceDir/$file")) { stream =>
          if (stream == null) {
            throw new IllegalStateException(s"Could not find resource $solrResourceDir/${LocalSolr.SolrConfDir}")
          }
          Files.copy(stream, (coreConfDir / file).path)
        }
      }

      // Load core
      logger.info(s"Creating core $name...")
      server.getCoreContainer.create(name, coreConfDir.path, Map.empty[String, String].asJava, true)
      true
    }
  }

  override def listIndexes: List[String] = home.children.filter(_.isDirectory).map(_.name).toList

  override def close(): Unit = server.close()
}

/** Companion object. */
object LocalSolr {

  /** Directory name for solr core config. */
  final val SolrConfDir = "conf"

  /** Name of solr config file. */
  final val SolrConfFile = "solr.xml"

  /** Default core name for embedded solr server. */
  final val DefaultCoreName = "theorydata"

  /** Creates new local solr repository.
    *
    * @param solrHome directory for solr instance to live in
    * @param solrResources resource folder where solr config files are stored, such as solr.xml, conf/ folder.
    * @return local solr repository
    */
  def apply(solrHome: JFile, solrResources: String = "solr"): LocalSolr = {
    val solrConfFile = s"$solrResources/$SolrConfFile"
    require(Resource.url(solrConfFile).isDefined)
    require(Resource.url(s"$solrResources/$SolrConfDir").isDefined)

    val home = File(solrHome.getAbsolutePath)
    require(home.isDirectory, s"Solr home $home does not exist")
    require(home.isWriteable, s"No write access to solr home directory $home")

    // Fill home
    Using.resource(Resource.getAsStream(solrConfFile)) { stream =>
      if (stream == null) {
        throw new IllegalArgumentException(s"Could not find resource $solrResources/$SolrConfFile")
      }
      Files.copy(stream, (home / SolrConfFile).path, StandardCopyOption.REPLACE_EXISTING)
    }

    val server = new EmbeddedSolrServer(home.path, DefaultCoreName)
    new LocalSolr(server, File(solrHome.getAbsolutePath), solrResources)
  }
}

/** Remote http solr client.
  *
  * @param client underlying solr connection
  * @param configSet name of the config set (that must be available on the instance)
  */
final class RemoteSolr private (private val client: SolrClient, private val configSet: String) extends SolrRepository {
  override def createIndex(name: String): Boolean = this.synchronized {
    if (!listIndexes.contains(name)) {
      val req = new CoreAdminRequest.Create()
      req.setConfigSet(configSet)
      req.setCoreName(s"${configSet}_$name")
      req.process(client)
      true
    } else {
      false
    }
  }

  override def listIndexes: List[String] = {
    val prefix = configSet + "_"
    val request = new CoreAdminRequest()
    request.setAction(CoreAdminAction.STATUS)

    val resp = request.process(client)

    resp.getCoreStatus.asScala
      .map(_.getKey)
      .toList
      .filter(_.startsWith(prefix))
      .map(_.stripPrefix(prefix))
  }

  override def request(request: SolrRequest[_ <: SolrResponse], collection: String): NamedList[AnyRef] =
    client.request(request, s"${configSet}_$collection")

  override def close(): Unit = client.close()
}

/** Companion object. */
object RemoteSolr {

  /** Creates a new remote solr repository.
    *
    * @param host address
    * @param port on which solr runs
    * @param configSet name of the config set (that must be available on the instance)
    * @return configured remote solr repository
    */
  def apply(host: String, port: Int, configSet: String): RemoteSolr =
    new RemoteSolr(
      new HttpSolrClient.Builder()
        .withBaseSolrUrl(new URL("http", host, port, "/solr").toString)
        .withConnectionTimeout(5 * 60 * 1000)
        .withSocketTimeout(5 * 60 * 1000)
        .build,
      configSet
    )
}

/** Remote solr cloud connection.
  *
  * @param client configured cloud solr client
  * @param configSet name of the config set (that must be available for the cluster)
  * @param numShards number of shards for new collections
  * @param numReplicas number of replicas for new collections
  */
final class CloudSolr private (
    private val client: CloudSolrClient,
    private val configSet: String,
    private val numShards: Int,
    private val numReplicas: Int)
    extends SolrRepository {
  override def createIndex(name: String): Boolean = this.synchronized {
    if (!listIndexes.contains(name)) {
      CollectionAdminRequest.createCollection(s"${configSet}_$name", configSet, numShards, numReplicas)
      true
    } else {
      false
    }
  }

  override def listIndexes: List[String] = {
    val prefix = configSet + "_"
    CollectionAdminRequest
      .listCollections(client)
      .asScala
      .toList
      .filter(_.startsWith(prefix))
      .map(_.stripPrefix(prefix))
  }

  override def request(request: SolrRequest[_ <: SolrResponse], collection: String): NamedList[AnyRef] =
    client.request(request, s"${configSet}_$collection")

  override def close(): Unit = client.close()
}

/** Companion object. */
object CloudSolr {

  /** Creates new Cloud Solr repository.
    *
    * @param zkhosts zookeeper hosts
    * @param configSet name of the config set (that must be available in the cluster)
    * @param numShards number of shards for new collections
    * @param numReplicas number of replicas for new collections
    * @return configured cloud solr connection
    */
  def apply(zkhosts: Seq[ZKHost], configSet: String, numShards: Int, numReplicas: Int): CloudSolr = {
    require(zkhosts.nonEmpty, "must have at least one zookeeper")
    val hosts: java.util.List[String] = zkhosts.map(zkhost => zkhost.host + zkhost.port.toString).toBuffer.asJava
    new CloudSolr(new CloudSolrClient.Builder(hosts).build, configSet, numShards, numReplicas)
  }
}

/** Container for zookeeper host identifier, consisting of host address and port.
  *
  * @param host zookeeper host address
  * @param port zookeeper port
  */
final case class ZKHost(host: String, port: Int) {
  require(host.nonEmpty, "host must not be empty")
  require(port > 0 && port < 65535, "port must be between 0 and 65535, was " + port)
}

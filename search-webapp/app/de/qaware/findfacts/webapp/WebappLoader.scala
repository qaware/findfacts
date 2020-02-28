package de.qaware.findfacts.webapp

import java.io.File

import _root_.controllers.{ApiHelpController, AssetsComponents}
import com.softwaremill.macwire.wire
import com.typesafe.config.Config
import de.qaware.findfacts.common.solr
import de.qaware.findfacts.common.solr.{CloudSolr, LocalSolr, RemoteSolr, SolrRepository, ZKHost}
import de.qaware.findfacts.core.solrimpl.SolrQueryModule
import de.qaware.findfacts.webapp.controllers.{HomeController, QueryController}
import de.qaware.findfacts.webapp.utils.JsonMappings
import org.apache.solr.client.solrj.SolrClient
import play.api.ApplicationLoader.Context
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import play.api.{Application, ApplicationLoader, BuiltInComponentsFromContext, ConfigLoader}
import play.filters.HttpFiltersComponents
import play.filters.csrf.CSRFFilter
import play.modules.swagger.{SwaggerPlugin, SwaggerPluginImpl}
import router.Routes

import scala.collection.JavaConverters._

/** Loader that can dynamically load wired up application. */
class WebappLoader extends ApplicationLoader {
  override def load(context: Context): Application = new WebappModule(context).application
}

/** DI Module for configured webapp.
  *
  * @param context play context
  */
class WebappModule(context: Context)
    extends BuiltInComponentsFromContext(context)
    with SolrQueryModule
    with AssetsComponents
    with HttpFiltersComponents {

  // Disable CSRF, as it is not needed in this application
  override def httpFilters: Seq[EssentialFilter] = {
    super.httpFilters.filterNot(_.getClass == classOf[CSRFFilter])
  }

  // Connect to remote solr.
  override lazy val solrClient: SolrClient =
    configuration.get[SolrRepository]("solr")(WebappModule.repositoryLoader).solrConnection()

  private val jsonMappings: JsonMappings = wire[JsonMappings]

  // Wire up controllers for the router
  private lazy val homeController: HomeController = wire[HomeController]
  private lazy val queryController: QueryController = wire[QueryController]
  private lazy val docsController: ApiHelpController = wire[ApiHelpController]

  // Swagger plugin creates api doc. Cannot be lazy as it needs to be initialized first for api discovery.
  private val swaggerPlugin: SwaggerPlugin = wire[SwaggerPluginImpl]

  // Wire up router, which provides the main entry point for play
  private lazy val routesPrefix = "/"
  override lazy val router: Router = wire[Routes]
}

/** Companion object. */
object WebappModule {

  final val SolrHome = "solrhome"
  final val Host = "host"
  final val Port = "port"
  final val ZKHost = "zkhost"
  final val ZKHosts = "zkhosts"

  /** Solr instance core. */
  final val SolrCore = "theorydata2"

  /** Configuration loader for zk hosts. */
  implicit val zkHostLoader: ConfigLoader[ZKHost] = (rootConfig: Config, path: String) => {
    val config = rootConfig.getConfig(path)
    solr.ZKHost(config.getString(Host), config.getInt(Port))
  }

  /** Configuration loader for all solr repositories. */
  implicit val repositoryLoader: ConfigLoader[SolrRepository] = (rootConfig: Config, path: String) => {
    val config = rootConfig.getConfig(path)
    if (config.hasPath(SolrHome)) {
      LocalSolr(new File(config.getString(SolrHome)), SolrCore)
    } else if (config.hasPath(Host) && config.hasPath(Host)) {
      RemoteSolr(config.getString(Host), config.getInt(Port), SolrCore)
    } else {
      CloudSolr(config.getObjectList(ZKHosts).asScala.map(c => zkHostLoader.load(c.toConfig, ZKHost)))
    }
  }
}

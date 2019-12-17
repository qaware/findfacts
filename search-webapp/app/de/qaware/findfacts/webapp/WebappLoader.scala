package de.qaware.findfacts.webapp

import java.net.URL

import _root_.controllers.{ApiHelpController, AssetsComponents}
import com.softwaremill.macwire.wire
import de.qaware.findfacts.common.solr.{RemoteSolr, SolrRepository}
import de.qaware.findfacts.core.solrimpl.SolrQueryModule
import de.qaware.findfacts.webapp.controllers.{HomeController, QueryController}
import de.qaware.findfacts.webapp.utils.JsonUrlCodec
import play.api.ApplicationLoader.Context
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import play.api.{Application, ApplicationLoader, BuiltInComponentsFromContext}
import play.filters.HttpFiltersComponents
import play.filters.csrf.CSRFFilter
import play.modules.swagger.{SwaggerPlugin, SwaggerPluginImpl}
import router.Routes

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
  override lazy val repository: SolrRepository = RemoteSolr(new URL("http://localhost:8983/solr/theorydata"))

  private val urlEncoder: JsonUrlCodec = wire[JsonUrlCodec]

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

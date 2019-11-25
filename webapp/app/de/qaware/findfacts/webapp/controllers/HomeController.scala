package de.qaware.findfacts.webapp.controllers

import com.google.inject.{Inject, Singleton}
import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.webapp.views
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents, Request}

/** This controller delivers the application's home page, where the elm SPA is embedded.
  *
  * @param cc components of this controller
  */
@Singleton class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  val logger = Logger[HomeController]

  /** Deliver application. */
  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    logger.info("hello")
    Ok(views.html.index())
  }

  /** Redirects to swagger webjar index. */
  def redirectDocs(): Action[AnyContent] =
    Action { implicit request: Request[AnyContent] =>
      Redirect("docs/index.html", Map("url" -> Seq("/swagger.json")))
    }
}

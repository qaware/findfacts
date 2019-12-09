package de.qaware.findfacts.webapp.controllers

import com.google.inject.{Inject, Singleton}
import de.qaware.findfacts.webapp.views
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents, Request}

/** This controller delivers the application's home page, where the elm SPA is embedded.
  *
  * @param cc components of this controller
  */
final class HomeController (cc: ControllerComponents) extends AbstractController(cc) {

  /** Deliver application. */
  def index(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  /** Redirects to swagger webjar index. */
  def redirectDocs(): Action[AnyContent] =
    Action { implicit request: Request[AnyContent] =>
      Redirect("docs/index.html", Map("url" -> Seq("/swagger.json")))
    }
}

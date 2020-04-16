package de.qaware.findfacts.webapp.filters

import akka.stream.Materializer
import play.api.Logging
import play.api.mvc.{Filter, RequestHeader, Result}

import scala.concurrent.{ExecutionContext, Future}

/** Filter to log request path, timing, and status.
  * Code from https://www.playframework.com/documentation/2.7.x/ScalaHttpFilters, for some reason this is not actually
  * included in the play sources.
  *
  * @param mat stream materializer for Filter
  * @param ctx execution context of request
  */
class StatsLoggingFilter(implicit val mat: Materializer, ctx: ExecutionContext) extends Filter with Logging {
  override def apply(nextFilter: RequestHeader => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    val startTime = System.currentTimeMillis

    nextFilter(requestHeader).map { result =>
      val endTime = System.currentTimeMillis
      val requestTime = endTime - startTime

      logger.info(
        s"${requestHeader.method} ${requestHeader.uri} took ${requestTime}ms and returned ${result.header.status}")

      result.withHeaders("Request-Time" -> requestTime.toString)
    }
  }
}

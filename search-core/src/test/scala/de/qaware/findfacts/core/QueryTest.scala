package de.qaware.findfacts.core

import de.qaware.findfacts.common.dt.EtField
import io.circe.generic.auto._
import io.circe.syntax._
import org.scalatest.{FunSuite, Matchers}

class QueryTest extends FunSuite with Matchers {
  test("Json encoding/decoding for filter query") {
    val query: FilterQuery = FilterQuery(Filter(Map(EtField.Name -> StringExpression("*gauss*"))), 10)
    val json = query.asJson
    json.as[FilterQuery] should equal(Right(query))
  }

  test("Json encoding/decoding for facet query") {
    val query: FacetQuery = FacetQuery(Filter(Map(EtField.Name -> StringExpression("*gauss*"))), Set(EtField.Kind), 100)
    val json = query.asJson
    json.as[FacetQuery] should equal(Right(query))
  }
}

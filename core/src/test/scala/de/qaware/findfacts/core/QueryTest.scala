package de.qaware.findfacts.core

import de.qaware.findfacts.common.dt.EtField
import io.circe.generic.auto._
import io.circe.syntax._
import org.scalatest.{FunSuite, Matchers}

class QueryTest extends FunSuite with Matchers {
  test("Json encoding/decoding for filter query") {
    val query: Query = FilterQuery(Filter(Map(EtField.Name -> StringExpression("*gauss*"))), 10)
    val json = query.asJson
    json.as[Query] should equal(Right(query))
  }

  test("Json encoding/decoding for facet query") {
    val query: Query = FacetQuery(Filter(Map(EtField.Name -> StringExpression("*gauss*"))), EtField.Kind)
    val json = query.asJson
    json.as[Query] should equal(Right(query))
  }
}

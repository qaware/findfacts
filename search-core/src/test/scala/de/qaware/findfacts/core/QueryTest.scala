package de.qaware.findfacts.core

import io.circe.generic.auto._
import io.circe.syntax._
import org.scalatest.{FunSuite, Matchers}

import de.qaware.findfacts.common.dt.EtField

class QueryTest extends FunSuite with Matchers {
  test("Json encoding/decoding for filter query") {
    val query: FilterQuery = FilterQuery(List(FieldFilter(EtField.Name, Exact("*gauss*"))), 10)
    val json = query.asJson
    json.as[FilterQuery] should equal(Right(query))
  }

  test("Json encoding/decoding for facet query") {
    val query: FacetQuery = FacetQuery(List(FieldFilter(EtField.Name, Exact("*gauss*"))), Set(EtField.Kind), 100)
    val json = query.asJson
    json.as[FacetQuery] should equal(Right(query))
  }
}

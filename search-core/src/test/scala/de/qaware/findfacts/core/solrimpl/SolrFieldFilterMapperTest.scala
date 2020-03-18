package de.qaware.findfacts.core.solrimpl

import scala.util.{Success, Try}
import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.{FieldFilter, Filter, Term}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers}

class SolrFieldFilterMapperTest extends FunSuite with Matchers with MockFactory {
  trait test {
    implicit val queryService: SolrQueryService = mock[SolrQueryService]
    implicit val index: String = "default"
    val filterMapper: SolrFilterMapper = mock[SolrFilterMapper]

    val sut = new SolrFieldFilterMapper(filterMapper)
  }

  test("Test mapping")(new test {
    val query = List(
      FieldFilter(EtField.Name, Term("t1")),
      FieldFilter(EtField.SourceTheory, Term("t2")),
      FieldFilter(EtField.Kind, Term("t3")))

    // Setup mock
    (filterMapper.mapFilter(_: Filter)(_: String, _: SolrQueryService))
      .expects(Term("t1"), index, queryService)
      .returning(Success("f1"))
    (filterMapper.mapFilter(_: Filter)(_: String, _: SolrQueryService))
      .expects(Term("t2"), index, queryService)
      .returning(Success("f2"))
    (filterMapper.mapFilter(_: Filter)(_: String, _: SolrQueryService))
      .expects(Term("t3"), index, queryService)
      .returning(Success("f3"))

    val filters: Try[Filters] = sut.mapFieldFilters(query)

    filters.get.fqs should contain theSameElementsAs List("{!tag=top,theory}theory:f2")
    filters.get.childFqs should contain theSameElementsAs List("{!tag=name}name:f1", "{!tag=thy_kind}thy_kind:f3")
  })
}

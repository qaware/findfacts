package de.qaware.findfacts.core.solrimpl

import scala.util.{Success, Try}

import org.mockito.{ArgumentMatchersSugar, MockitoSugar}
import org.scalatest.{FunSuite, Matchers}

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.{FieldFilter, Term}

class SolrFieldFilterMapperTest extends FunSuite with Matchers with MockitoSugar with ArgumentMatchersSugar {

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
    when(filterMapper.mapFilter(Term("t1"))(index, queryService)) thenReturn Success("f1")
    when(filterMapper.mapFilter(Term("t2"))(index, queryService)) thenReturn Success("f2")
    when(filterMapper.mapFilter(Term("t3"))(index, queryService)) thenReturn Success("f3")

    val filters: Try[Filters] = sut.mapFieldFilters(query)

    verify(filterMapper, times(3)).mapFilter(any)(any, any)
    filters.get.fqs should contain theSameElementsAs List("{!tag=top,theory}theory:f2")
    filters.get.childFqs should contain theSameElementsAs List("{!tag=name}name:f1", "{!tag=thy_kind}thy_kind:f3")
  })
}

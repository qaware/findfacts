package de.qaware.findfacts.core.solrimpl

import scala.collection.mutable
import scala.util.Try

import de.qaware.findfacts.common.utils.TryUtils._
import de.qaware.findfacts.core.{AbstractFQ, Filter, FilterComplement, FilterIntersection, FilterUnion}

/** Maps filter queries to solr query strings.
  *
  * @param termMapper to map filter terms
  */
class SolrAbstractFqMapper(termMapper: SolrFilterTermMapper) {

  /** Builds filter query string. Solr filter caches can be configured to cache the result for each string.
    *
    * @param filter query
    * @param qParams additional query parameters
    * @param queryService for recursive calls
    * @return query strings that can be cached individually
    */
  def buildFilter(filter: AbstractFQ)(
      implicit qParams: mutable.Map[String, Seq[String]],
      queryService: SolrQueryService): Try[String] = filter match {
    case Filter(fieldTerms) =>
      val (childFieldTerms, parentFieldTerms) = fieldTerms.toSeq.partition(!_._1.isParent)

      val childTerms: Try[Seq[String]] = childFieldTerms.map(e => termMapper.mapFilterTerm(e._1, e._2))
      val parentTerms: Try[Seq[String]] = parentFieldTerms.map(e => termMapper.mapFilterTerm(e._1, e._2))

      for {
        childTerms <- childTerms
        parentTerms <- parentTerms
      } yield {
        val parentQuery = if (parentTerms.nonEmpty) parentTerms.mkString(SolrQueryLiterals.And) else ""
        if (childTerms.nonEmpty) {
          val name = s"cfq${qParams.size}"
          qParams.put(name, childTerms)
          val childQuery = s"({!parent which=${SolrQueryLiterals.QueryParent} filters=" + "$" + name + "})"
          if (parentTerms.isEmpty) childQuery else s"$parentQuery${SolrQueryLiterals.And}$childQuery"
        } else {
          if (parentTerms.isEmpty) SolrQueryLiterals.QueryAll else parentQuery
        }
      }
    case FilterIntersection(f1, f2, fn @ _*) =>
      val filters: Try[Seq[String]] = (f1 +: f2 +: fn).map(buildFilter)
      filters.map(fs => s"(${fs.mkString(SolrQueryLiterals.And)})")
    case FilterUnion(f1, f2, fn @ _*) =>
      val filters: Try[Seq[String]] = (f1 +: f2 +: fn).map(buildFilter)
      filters.map(fs => s"(${fs.mkString(SolrQueryLiterals.Or)})")
    case FilterComplement(filter) => buildFilter(filter).map(f => s"(${SolrQueryLiterals.Not}$f)")
  }
}

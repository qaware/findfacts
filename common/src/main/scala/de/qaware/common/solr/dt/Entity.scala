package de.qaware.common.solr.dt

import org.apache.solr.client.solrj.beans.Field
import java.io.File
import SolrSchema._

case class Doc(@Field(ID) id: String)

case class SourcePosition(sourceFile: File, startPos: Int, endPos: Int)

case class BaseEntity(id: Long, sourcePosition: SourcePosition)

//case class Doc(baseEntity: BaseEntity, text: String)

case class Const(baseEntity: BaseEntity, uses: List[Long], name: String, const_type: String)

case class Term(baseEntity: BaseEntity, uses: List[Long])

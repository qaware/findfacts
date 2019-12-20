package de.qaware.findfacts.theoryimporter.steps.impl

import de.qaware.findfacts.common.solr.{ConstRecord, FactRecord, TypeRecord}
import de.qaware.findfacts.theoryimporter.TheoryView.Theory
import de.qaware.findfacts.theoryimporter.steps.{ImportError, ImportStep, StepContext}

/** Step to find related entities, i.e. entities that spring from the same source positions. */
class FindRelatedStep extends ImportStep {
  override def apply(theories: Seq[Theory])(implicit ctx: StepContext): List[ImportError] = {
    // Group by position
    val entitiesByPos = ctx.theoryEntities.groupBy(e => (e.sourceFile, e.startPosition, e.endPosition))

    entitiesByPos.keySet foreach { key =>
      val entities = entitiesByPos(key)
      val allIds = entities.map(_.id)

      // Add every ID at this position except self.id to entities
      entities foreach {
        case entity: ConstRecord => ctx.updateEntity(entity, entity.copy(related = (allIds - entity.id).toArray))
        case entity: FactRecord => ctx.updateEntity(entity, entity.copy(related = (allIds - entity.id).toArray))
        case entity: TypeRecord => ctx.updateEntity(entity, entity.copy(related = (allIds - entity.id).toArray))
      }
    }

    List.empty
  }
}

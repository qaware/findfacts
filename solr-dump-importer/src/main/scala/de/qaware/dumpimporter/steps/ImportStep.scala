package de.qaware.dumpimporter.steps

import scala.collection.mutable

import de.qaware.common.solr.dt.Entity.Id
import de.qaware.common.solr.dt.{ConstEntity, DocumentationEntity, Entity, FactEntity, TypeEntity}
import de.qaware.dumpimporter.Config

/** Step of the import process. */
trait ImportStep {

  /** Configuration parameter */
  val config: Config

  /** Applies the step, mutating the context.
    *
    * @param ctx context that is mutated in the step
    */
  def apply(ctx: StepContext): Unit
}

/** Case class to hold mutable context shared throughout the steps.
  *
  * @param serialsById serial id for each entity, consistent for a single run
  * @param consts intermediate constant entities
  * @param types intermediate type entities
  * @param facts intermediate fact entities
  * @param doc intermediate documentation entities
  */
final case class StepContext(
    serialsById: mutable.MultiMap[Id, Long] = new mutable.HashMap[Id, mutable.Set[Long]]
      with mutable.MultiMap[Id, Long],
    consts: mutable.Set[ConstEntity] = mutable.Set.empty,
    types: mutable.Set[TypeEntity] = mutable.Set.empty,
    facts: mutable.Set[FactEntity] = mutable.Set.empty,
    doc: mutable.Set[DocumentationEntity] = mutable.Set.empty) {

  private def addToSet(entity: Entity) = entity match {
    case e: ConstEntity => consts.add(e)
    case t: TypeEntity => types.add(t)
    case f: FactEntity => facts.add(f)
    case _: Any => throw new IllegalArgumentException("Not a stored entity type")
  }

  private def removeFromSet(entity: Entity) = entity match {
    case e: ConstEntity => consts.remove(e)
    case t: TypeEntity => types.remove(t)
    case f: FactEntity => facts.remove(f)
    case _: Any => throw new IllegalArgumentException("Not a stored entity type")
  }

  /** Adds an entity and all its isabelle serials to the context.
    *
    * @param entity to add
    * @param serials of corresponding isabelle entities
    */
  def addEntity(entity: Entity, serials: Seq[Long]): Unit = {
    serials.map(serialsById.addBinding(entity.id, _))
    addToSet(entity)
  }

  /** Updates an entity in the context.
    *
    * @param old entity object
    * @param entity updated
    */
  def updateEntity(old: Entity, entity: Entity): Unit = {
    // Id has to remain stable!
    if (old.id != entity.id) {
      throw new IllegalArgumentException("Id must not change when updating entities!")
    }
    val serials = serialsById(old.id)
    serialsById.remove(old.id)
    serialsById.put(entity.id, serials)
    removeFromSet(old)
    addToSet(entity)
  }

  /** Gives a set of all entities.
    *
    * @return a set of all entities
    */
  def allEntities: mutable.Set[Entity] = consts ++ facts ++ types ++ doc
}
object StepContext {

  /** Builds an empty context.
    *
    * @return a new context
    */
  def empty: StepContext = new StepContext()
}

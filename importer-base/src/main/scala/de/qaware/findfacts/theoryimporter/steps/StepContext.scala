package de.qaware.findfacts.theoryimporter.steps

import scala.collection.mutable

import de.qaware.findfacts.common.dt.EtField.Children
import de.qaware.findfacts.common.dt._

/** Holds mutable context shared throughout the steps. */
final class StepContext private (
    private val _blocks: mutable.Set[BlockEt] = mutable.Set.empty,
    private val _consts: mutable.Set[ConstantEt] = mutable.Set.empty,
    private val _types: mutable.Set[TypeEt] = mutable.Set.empty,
    private val _facts: mutable.Set[FactEt] = mutable.Set.empty,
    private val _docs: mutable.Set[DocumentationEt] = mutable.Set.empty) {

  private def addToSet(entity: BaseEt): Unit = entity match {
    case d: DocumentationEt => _docs.add(d)
    case b: BlockEt => _blocks.add(b)
    case e: ConstantEt => _consts.add(e)
    case t: TypeEt => _types.add(t)
    case f: FactEt => _facts.add(f)
  }

  private def removeFromSet(entity: BaseEt): Unit = entity match {
    case d: DocumentationEt => _docs.remove(d)
    case b: BlockEt => _blocks.remove(b)
    case e: ConstantEt => _consts.remove(e)
    case t: TypeEt => _types.remove(t)
    case f: FactEt => _facts.remove(f)
  }

  /** Adds an entity to the context.
    *
    * @param entity to add
    */
  def addEntity(entity: TheoryEt, blockEt: BlockEt): Unit = {
    addToSet(entity)

    // Update parent block
    _blocks.find(_.id == blockEt.id) match {
      case Some(block) => updateEntity(block, block.copy(entities = Children.apply(block.entities :+ entity)))
      case None => addToSet(blockEt.copy(entities = Children.apply(List(entity))))
    }
  }

  def addEntity(docEt: DocumentationEt): Unit = {
    addToSet(docEt)
  }

  /** Updates an entity in the context.
    *
    * @param old entity object
    * @param entity updated
    */
  def updateEntity(old: BaseEt, entity: BaseEt): Unit = {
    // Id has to remain stable!
    if (old.id != entity.id) {
      throw new IllegalArgumentException("Id must not change when updating entities!")
    }
    removeFromSet(old)
    addToSet(entity)
  }

  def blocks: Set[BlockEt] = _blocks.to[Set]

  /** Accessor for immutable view on consts.
    *
    * @return immutable consts set
    */
  def consts: Set[ConstantEt] = _consts.to[Set]

  /** Accessor for immutable view on types.
    *
    * @return immutable types set
    */
  def types: Set[TypeEt] = _types.to[Set]

  /** Accessor for immutable view on facts.
    *
    * @return immutable facts set
    */
  def facts: Set[FactEt] = _facts.to[Set]

  /** Accessor for immutable view on docs.
    *
    * @return immutable docs set
    */
  def docs: Set[DocumentationEt] = _docs.to[Set]

  /** Gives an immutable set of all semantic theory entities.
    *
    * @return a set of all semantic theory entities
    */
  def theoryEntities: Set[TheoryEt] = consts ++ types ++ facts

  /** Gives an immutable set of all entities.
    *
    * @return a set of all entities
    */
  def allEntities: Set[BaseEt] = blocks ++ docs ++ theoryEntities
}
object StepContext {

  /** Builds an empty context.
    *
    * @return a new context
    */
  def apply(): StepContext = new StepContext()
}

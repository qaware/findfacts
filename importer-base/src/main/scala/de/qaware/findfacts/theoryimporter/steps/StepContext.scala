package de.qaware.findfacts.theoryimporter.steps

import scala.collection.mutable

import de.qaware.findfacts.common.dt._

/** Holds mutable context shared throughout the steps. Stores parent-child relation and keeps it consistent. */
final class StepContext private (
    private val _blocks: mutable.Set[BlockEt] = mutable.Set.empty,
    private val _docs: mutable.Set[DocumentationEt] = mutable.Set.empty,
    private val _consts: mutable.Map[ConstantEt, String] = mutable.Map.empty,
    private val _types: mutable.Map[TypeEt, String] = mutable.Map.empty,
    private val _facts: mutable.Map[FactEt, String] = mutable.Map.empty) {

  private def addToMap(entity: TheoryEt, blockId: String): Unit = entity match {
    case c: ConstantEt => _consts.put(c, blockId)
    case f: FactEt => _facts.put(f, blockId)
    case t: TypeEt => _types.put(t, blockId)
  }

  private def remove(entity: BaseEt): Unit = entity match {
    case d: DocumentationEt => _docs.remove(d)
    case b: BlockEt => _blocks.remove(b)
    case c: ConstantEt => _consts.remove(c)
    case f: FactEt => _facts.remove(f)
    case t: TypeEt => _types.remove(t)
  }

  /** Adds an entity to the context.
    *
    * @param entity to add
    */
  def addEntity(entity: TheoryEt, blockEt: BlockEt): Unit = {
    _blocks.add(blockEt)
    addToMap(entity, blockEt.id)
  }

  /** Adds an documentation entity to the context.
    *
    * @param docEt to add
    */
  def addEntity(docEt: DocumentationEt): Unit = {
    _docs.add(docEt)
  }

  /** Updates an entity in the context.
    *
    * @param old entity object
    * @param entity updated
    * @tparam A type of the entity
    */
  def updateEntity[A <: BaseEt](old: A, entity: A): Unit = {
    // Id has to remain stable!
    if (old.id != entity.id) {
      throw new IllegalArgumentException("Id must not change when updating entities!")
    }

    old match {
      case et: TheoryEt =>
        val blockId = et match {
          case c: ConstantEt => _consts(c)
          case f: FactEt => _facts(f)
          case t: TypeEt => _types(t)
        }
        remove(old)
        addToMap(entity.asInstanceOf[TheoryEt], blockId)
      case _ =>
        remove(old)
        entity match {
          case b: BlockEt => _blocks.add(b)
          case d: DocumentationEt => _docs.add(d)
        }
    }
  }

  /** Accessor for immutable view on blocks. Method call is expensive, as parent-child-relationship has to be populated.
    *
    * @return immutable blocks set
    */
  def blocks: Set[BlockEt] = {
    val groupedTEs = (_consts ++ _types ++ _facts).groupBy(_._2).mapValues(_.keySet)
    _blocks.to[Set].map(block => block.copy(entities = groupedTEs(block.id).toList))
  }

  /** Accessor for immutable view on consts.
    *
    * @return immutable consts set
    */
  def consts: Set[ConstantEt] = _consts.keys.to[Set]

  /** Accessor for immutable view on types.
    *
    * @return immutable types set
    */
  def types: Set[TypeEt] = _types.keys.to[Set]

  /** Accessor for immutable view on facts.
    *
    * @return immutable facts set
    */
  def facts: Set[FactEt] = _facts.keys.to[Set]

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

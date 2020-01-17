package de.qaware.findfacts.theoryimporter.steps

import scala.collection.mutable

import de.qaware.findfacts.common.dt.{BaseEt, BlockEt, ConstantEt, DocumentationEt, FactEt, TheoryEt, TypeEt}

/** Holds mutable context shared throughout the steps. Stores parent-child relation and keeps it consistent.
  *
  * @param _blocks block entities, without children
  * @param _docs documentation entities
  * @param _consts pairs of constants and the block it is in
  * @param _types pairs of types and the block it is in
  * @param _facts pairs of facts and the block it is in
  */
final class StepContext private (
    private val _blocks: mutable.Set[BlockEt] = mutable.Set.empty,
    private val _docs: mutable.Set[DocumentationEt] = mutable.Set.empty,
    private val _consts: mutable.Set[(ConstantEt, String)] = mutable.Set.empty,
    private val _types: mutable.Set[(TypeEt, String)] = mutable.Set.empty,
    private val _facts: mutable.Set[(FactEt, String)] = mutable.Set.empty) {

  private def addToMap(entity: TheoryEt, blockId: String): Unit = entity match {
    case c: ConstantEt => _consts.add((c, blockId))
    case f: FactEt => _facts.add((f, blockId))
    case t: TypeEt => _types.add((t, blockId))
  }

  private def remove(entity: BaseEt): Unit = entity match {
    case d: DocumentationEt => _docs.remove(d)
    case b: BlockEt => _blocks.remove(b)
    case c: ConstantEt => _consts.filter(_._1 == c).map(_consts.remove)
    case f: FactEt => _facts.filter(_._1 == f).map(_facts.remove)
    case t: TypeEt => _types.filter(_._1 == t).map(_types.remove)
  }

  /** Adds an entity to the context.
    *
    * @param entity to add
    * @param blockEt parent block
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
    */
  def updateEntity(old: BaseEt, entity: BaseEt): Unit = {
    // Id and Kind have to remain stable!
    if (old.id != entity.id) {
      throw new IllegalArgumentException(s"Id must not change when updating entities! $old, $entity")
    }
    if (old.getClass != entity.getClass) {
      throw new IllegalArgumentException(s"Type must not change when updating entities! $old, $entity")
    }

    (old, entity) match {
      case (et: TheoryEt, newEt: TheoryEt) =>
        val containingBlocks = et match {
          case c: ConstantEt =>
            _consts.filter(_._1 == c).map(_._2)
          case f: FactEt =>
            _facts.filter(_._1 == f).map(_._2)
          case t: TypeEt =>
            _types.filter(_._1 == t).map(_._2)
        }
        if (containingBlocks.isEmpty) {
          throw new IllegalArgumentException(s"Entity $old to update does not exist")
        }
        remove(old)
        containingBlocks.foreach(addToMap(newEt, _))
      case _ =>
        remove(old)
        (entity: @unchecked) match {
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
    val entitiesByBlock = (_consts ++ _facts ++ _types).groupBy(_._2).mapValues(_.map(_._1))
    _blocks.to[Set].map(block => block.copy(entities = entitiesByBlock(block.id).toList))
  }

  /** Accessor for immutable view on consts.
    *
    * @return immutable consts set
    */
  def consts: Set[ConstantEt] = _consts.map(_._1).to[Set]

  /** Accessor for immutable view on types.
    *
    * @return immutable types set
    */
  def types: Set[TypeEt] = _types.map(_._1).to[Set]

  /** Accessor for immutable view on facts.
    *
    * @return immutable facts set
    */
  def facts: Set[FactEt] = _facts.map(_._1).to[Set]

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

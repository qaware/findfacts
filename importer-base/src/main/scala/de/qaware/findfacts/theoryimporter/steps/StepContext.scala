package de.qaware.findfacts.theoryimporter.steps

import scala.collection.mutable

import de.qaware.findfacts.common.dt.{BaseEt, BlockEt, ConstantEt, DocumentationEt, FactEt, TheoryEt, TypeEt}
import de.qaware.findfacts.common.utils.DefaultMultiMap

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
    private val _consts: mutable.MultiMap[ConstantEt, String] = DefaultMultiMap.empty,
    private val _types: mutable.MultiMap[TypeEt, String] = DefaultMultiMap.empty,
    private val _facts: mutable.MultiMap[FactEt, String] = DefaultMultiMap.empty) {

  private def put(entity: TheoryEt, blocks: mutable.Set[String]): Unit = entity match {
    case c: ConstantEt => _consts.put(c, blocks)
    case f: FactEt => _facts.put(f, blocks)
    case t: TypeEt => _types.put(t, blocks)
  }

  private def addToMap(entity: TheoryEt, blockId: String): Unit = entity match {
    case c: ConstantEt => _consts.addBinding(c, blockId)
    case f: FactEt => _facts.addBinding(f, blockId)
    case t: TypeEt => _types.addBinding(t, blockId)
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
            _consts.getOrElse(c, mutable.Set.empty)
          case f: FactEt =>
            _facts.getOrElse(f, mutable.Set.empty)
          case t: TypeEt =>
            _types.getOrElse(t, mutable.Set.empty)
        }
        if (containingBlocks.isEmpty) {
          throw new IllegalArgumentException(s"Entity $old to update does not exist")
        }
        remove(old)
        put(newEt, containingBlocks)
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
    val explodedMap: Seq[(TheoryEt, String)] = (_consts ++ _facts ++ _types).toSeq.flatMap(e => e._2.map((e._1, _)))
    val entitiesByBlock: Map[String, Set[TheoryEt]] = explodedMap.groupBy(_._2).mapValues(_.map(_._1).toSet)

    _blocks.to[Set].map(block => block.copy(entities = entitiesByBlock(block.id).toList))
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

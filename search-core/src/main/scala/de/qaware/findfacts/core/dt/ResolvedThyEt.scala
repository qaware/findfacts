package de.qaware.findfacts.core.dt

import de.qaware.findfacts.common.dt.Kind

/** Theory entity types with resolved relations. */
sealed trait ResolvedThyEt

/**
 * Resolved information for constants.
 *
 * @param id unique identifier
 * @param typ of the constant
 * @param uses referenced entities
 */
final case class ResolvedConstant(id: String, typ: String, uses: List[ShortThyEt], kind: Kind = Kind.Constant)
  extends ResolvedThyEt

/**
 * Resolved information for facts.
 *
 * @param id unique identifier
 * @param uses referenced entities
 */
final case class ResolvedFact(id: String, uses: List[ShortThyEt], kind: Kind = Kind.Fact) extends ResolvedThyEt

/**
 * Resolved information for types.
 *
 * @param id unique identifier
 * @param uses referenced entities
 */
final case class ResolvedType(id: String, uses: List[ShortThyEt], kind: Kind = Kind.Type) extends ResolvedThyEt

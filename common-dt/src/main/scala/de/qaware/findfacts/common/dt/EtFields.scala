package de.qaware.findfacts.common.dt

import shapeless.tag.@@

sealed trait EtField {
  type FieldType
  def fromString(s: String): FieldType = ??? // TODO implement
}

// TODO make real Enum
/** Typelevel representation of entity fields. */
object EtFields {

  /** Unique id. */
  object Id extends EtField {
    override type FieldType = String @@ this.type
  }

  /** Kind of the entity. Possible values in [[Kind]]. */
  object Kind extends EtField {
    type FieldType = EtKind.Value @@ this.type
  }

  /** Source theory file containing the entity. */
  object SourceFile extends EtField {
    type FieldType = String @@ this.type
  }

  /** Start position of entity definition, in absolute isabelle characters. */
  object StartPosition extends EtField {
    type FieldType = Int @@ this.type
  }

  /** End position of entity definition, in absolute isabelle characters. */
  object EndPosition extends EtField {
    type FieldType = Int @@ this.type
  }

  /** Long (in most cases fully-qualified) name. */
  object Name extends EtField {
    type FieldType = String @@ this.type
  }

  /** Proposition, of a constants, fact, or type entity. */
  object Proposition extends EtField {
    type FieldType = String @@ this.type
  }

  /** Source text in isabelle thy. */
  object SourceText extends EtField {
    type FieldType = String @@ this.type
  }

  /** Type of a constant entity. */
  object ConstantType extends EtField {
    type FieldType = String @@ this.type
  }

  /** Other entities that this entity uses in its propositions. */
  object PropositionUses extends EtField {
    type FieldType = Array[String] @@ this.type
  }

  /** Other entities that this entity uses in its types. */
  object TypeUses extends EtField {
    type FieldType = Array[String] @@ this.type
  }

  /** Other entities that are generated from the same source position as this entity. */
  object Related extends EtField {
    type FieldType = Array[String] @@ this.type
  }

  /** Other entities that this entity uses in its proofs. */
  object ProofUses extends EtField {
    type FieldType = Array[String] @@ this.type
  }

  /** Kind of a documentation entity. Possible values in [[DocumentationKind]].  */
  object DocumentationKind extends EtField {
    type FieldType = DocKind.Value @@ this.type
  }
}

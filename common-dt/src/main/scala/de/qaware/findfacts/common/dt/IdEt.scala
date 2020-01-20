package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.dt.EtField.{ChildIds, Id}

/** Parent entity containing child ids.
  *
  * @param children containing ids only
  */
case class IdEt(children: ChildIds.FieldType)

/** Child id wrapper.
  *
  * @param id of a child
  */
case class IdChild(id: Id.FieldType)

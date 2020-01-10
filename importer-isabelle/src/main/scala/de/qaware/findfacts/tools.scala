/*  Title:      findfacts/tools.scala
    Author:     Fabian Huch, TU Munich/QAware GmbH

Findfacts isabelle tool registration.
*/

package de.qaware.findfacts


import isabelle._


/* Companion object for sbt entry point */

object Tools extends App

/* Tools of this component */

class Tools extends Isabelle_Scala_Tools(Importer.isabelle_tool)

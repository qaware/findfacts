package de.qaware.findfacts.it

import fastparse.NoWhitespace._
import fastparse._

/** Parsed spec.
  *
  * @param name of the test
  * @param testCode scala test code
  * @param thyCode Isabelle theory code
  * @param specLine line at which spec starts
  */
case class Spec(name: String, testCode: String, thyCode: String, specLine: Int)

/** Parser of spec theory files. */
object SpecTestParser {
  // Tokens
  private final val CStart = "(*"
  private final val CEnd = "*)"
  private final val Begin = "SPEC:BEGIN"
  private final val Verify = "SPEC:VERIFY"
  private final val End = "SPEC:END"

  // Parser
  def file[_: P]: P[Seq[Spec]] = P(line.rep.! ~ (spec ~ line.rep.!).rep) map {
    case (str, tuples) =>
      var idx = str.linesIterator.length
      tuples.map {
        case (spec, str) =>
          val s = spec.copy(specLine = spec.specLine + idx)
          idx += (spec.thyCode + spec.testCode + str).linesIterator.length + 3
          s
      }
  }

  private def spec[_: P]: P[Spec] = P(specBegin ~ line.rep.! ~ specBody) map {
    case (name, code, tests) => Spec(name, tests, code, 0)
  }

  private def specBegin[_: P]: P[String] =
    P(ws ~ CStart ~ ws ~ Begin ~ CharIn(":") ~ AnyChar.rep(1).! ~ CEnd ~ ws ~ eol)
  private def specBody[_: P]: P[String] = P(specVerify ~ line.rep.! ~ specEnd)

  private def specVerify[_: P]: P[Unit] = P(ws ~ CStart ~ ws ~ Verify ~ ws ~ eol)
  private def specEnd[_: P]: P[Unit] = P(ws ~ End ~ ws ~ CEnd ~ ws ~ eol)

  private def line[_: P]: P[String] = P(!(specBegin | specVerify | specEnd) ~ (!eol ~ AnyChar).rep ~ eol).!

  private def ws[_: P]: P[Unit] = P(CharIn(" ", "\t").rep)
  private def eol[_: P]: P[Unit] = P("\n" | "\r\n" | End)
}

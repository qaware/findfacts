package de.qaware.findfacts.importer

import fastparse.NoWhitespace._
import fastparse._

/**
 * Parsed spec.
 *
 * @param name of the test
 * @param testCode scala test code
 * @param thyCode Isabelle theory code
 * @param specStartLine line at which spec starts
 */
case class Spec(name: String, testCode: String, thyCode: String, specStartLine: Int)

/** Parser of spec theory files. */
object SpecTestParser {
  def parse(input: String): Either[String, Seq[Spec]] = {
    fastparse.parse(input, file(_), verboseFailures = true) match {
      case Parsed.Success(value, _) => Right(value)
      case Parsed.Failure(label, index, _) => Left(s"Parse error at $index: $label")
    }
  }

  // Tokens
  private final val CSTART = "(*"
  private final val CEND = "*)"
  private final val SPEC_BEGIN = "SPEC:BEGIN"
  private final val SPEC_VERIFY = "SPEC:VERIFY"
  private final val SPEC_END = "SPEC:END"

  // Parser
  private def file[_: P]: P[Seq[Spec]] =
    P(line.rep.! ~/ (spec ~/ line.rep.!).rep ~ End) map {
      case (str, tuples) =>
        var idx = str.linesIterator.length
        tuples.map {
          case (spec, str) =>
            val s = spec.copy(specStartLine = spec.specStartLine + idx)
            idx += (spec.thyCode + spec.testCode + str).linesIterator.length + 3
            s
        }
    }

  private def spec[_: P]: P[Spec] =
    P(specBegin ~/ line.rep.! ~/ specBody) map {
      case (name, code, tests) => Spec(name, tests, code, 1)
    }

  private def specBegin[_: P]: P[String] =
    P(ws ~ CSTART ~ ws ~ SPEC_BEGIN ~/ ":" ~/ (!CEND ~ AnyChar).rep(1).! ~/ CEND ~/ ws ~/ eol)

  private def specBody[_: P]: P[String] = P(specVerify ~/ line.rep.! ~/ specEnd)

  private def specVerify[_: P]: P[Unit] = P(ws ~ CSTART ~ ws ~ SPEC_VERIFY ~/ ws ~/ eol)

  private def specEnd[_: P]: P[Unit] = P(ws ~ SPEC_END ~ ws ~/ CEND ~/ ws ~/ (eol | &(End)))

  private def line[_: P]: P[String] = P(!(specBegin | specVerify | specEnd) ~ (newLine | lastLine)).!

  private def newLine[_: P]: P[String] = P((!eol ~ AnyChar).rep ~ eol).!

  private def lastLine[_: P]: P[String] = P((!End ~ AnyChar).rep(1) ~ &(End)).!

  private def ws[_: P]: P[Unit] = P(CharIn(" ", "\t").rep)

  private def eol[_: P]: P[Unit] = P(StringIn("\n", "\r\n"))
}

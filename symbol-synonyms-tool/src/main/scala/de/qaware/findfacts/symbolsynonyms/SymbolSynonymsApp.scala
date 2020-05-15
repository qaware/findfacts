package de.qaware.findfacts.symbolsynonyms

import scala.collection.mutable

import better.files.File
import com.typesafe.scalalogging.Logger
import scopt.{OptionParser, Read}

import de.qaware.findfacts.core.solrimpl.SolrFilterMapper

/**
 * Application config.
 *
 * @param file input file to read
 * @param out output file to write to, if any (otherwise write to logging out)
 */
final case class Config(file: File = File(""), out: Option[File] = None)

/** Simple app to parse yxml files. */
object SymbolSynonymsApp extends App {

  /** Regex to check if synonym might get split into multiple tokens. */
  val IsTokenDelimited = """.*[(){}\[\].,:"].*""".r.pattern.asMatchPredicate()

  /** Regex to check if synonym can't be in synonym file. */
  val IsSynonymDelimiter = """.*=>.*""".r.pattern.asMatchPredicate()

  /** Parse files from better.files */
  implicit val betterFileRead: Read[File] = Read.reads(File(_))

  private val builder = new OptionParser[Config]("symbol-synonyms") {
    head("Creates synonyms config")
    arg[File]("<dir>")
      .required()
      .text("isabelle symbols file to read - usually isabelle/etc/symbols")
      .action((file, conf) => conf.copy(file = file))
    opt[File]('o', "output-file")
      .valueName("<dir>")
      .text("output file")
      .action((file, conf) => conf.copy(out = Some(file)))
    checkConfig { c =>
      if (!c.file.exists || !c.file.isRegularFile) {
        failure("Must specify a valid input file")
      } else if (c.out.exists(_.isDirectory)) {
        failure("Output must not be directory")
      } else {
        success
      }
    }
  }

  private val logger = Logger[SymbolSynonymsApp.type]
  private val filterMapper = new SolrFilterMapper()

  /**
   * Synonyms.
   *
   * @param words that have synonyms
   * @param replacements for words
   */
  final case class Synonym(words: Seq[String], replacements: Seq[String]) {
    def write: String =
      s"${words.map(filterMapper.escape(_, exact = false)).mkString(",")} => ${replacements.mkString(",")}"
  }

  /** Regex for isabelle name and unicode codepoint. */
  val synonym = """^(\\<[^\s]*>)\s*code: 0x([0-9a-fA-F]*) .*""".r

  private def parseLine(line: String, uniqueAbbrevs: Set[String]): Option[Synonym] =
    line match {
      case synonym(isabelleToken, hex) =>
        val replacement = Character.toString(Integer.parseInt(hex, 16))
        val abbrevs = getAbbrevs(line).filter(uniqueAbbrevs.contains).filter { abbrev =>
          if (IsTokenDelimited.test(abbrev)) {
            logger.info(
              "<charFilter class=\"solr.PatternReplaceCharFilterFactory\" pattern=\"\\Q" + abbrev + "\\E\" replacement=\"" + replacement + "\"/>")
            false
          } else if (IsSynonymDelimiter.test(abbrev)) {
            logger.info(
              "<filter class=\"solr.PatternReplaceFilterFactory\" pattern=\"\\Q" + abbrev + "\\E\" replacement=\"" + replacement + "\"/>")
            false
          } else {
            true
          }
        }
        Some(Synonym(isabelleToken +: abbrevs, Seq(replacement)))
      case _ => None
    }

  private def getAbbrevs(s: String): List[String] = "abbrev: ([^\\s]+)".r.findAllMatchIn(s).map(_.group(1)).toList

  builder.parse(args, Config()) match {
    case Some(config) =>
      // Count occurrences of abbreviations - only unique abbreviations are interesting.
      val abbrevCounts = mutable.Map.empty[String, Int]
      getAbbrevs(config.file.contentAsString).foreach { abbrev =>
        abbrevCounts.get(abbrev) match {
          case Some(count) => abbrevCounts.update(abbrev, count + 1)
          case None => abbrevCounts.put(abbrev, 1)
        }
      }
      val uniqueAbbrevs = abbrevCounts.filter(_._2 == 1).keys.toSet
      logger.info("In schema, add the following:")
      val content = config.file.lines.flatMap(parseLine(_, uniqueAbbrevs)).map(_.write).mkString("\n")
      config.out match {
        case None => logger.info(s"Synonyms:\n$content")
        case Some(outFile) => outFile.write(content)
      }
    case _ =>
  }
}

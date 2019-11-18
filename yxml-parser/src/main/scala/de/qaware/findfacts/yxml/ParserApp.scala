package de.qaware.findfacts.yxml

import better.files.File
import com.typesafe.scalalogging.Logger
import scopt.{OptionParser, Read}

/** Application config.
  *
  * @param file input file to read
  * @param out output file to write to, if any (otherwise write to logging out)
  */
final case class Config(file: File = File(""), out: Option[File] = None)

/** Simple app to parse yxml files. */
object ParserApp extends App {

  /** Parse files from better.files */
  implicit val betterFileRead: Read[File] = Read.reads(File(_))

  private val builder = new OptionParser[Config]("yxml-parser") {
    head("Parses a yxml file")
    arg[File]("<dir>")
      .required()
      .text("file to parse")
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

  private val logger = Logger[ParserApp.type]

  builder.parse(args, Config()) match {
    case Some(config) =>
      YxmlParser(config.file.contentAsString) match {
        case Left(e) => logger.info("Could not parse file: {}", e)
        case Right(yxml) =>
          config.out match {
            case None => logger.info("Parsed content:\n{}", yxml)
            case Some(outFile) => outFile.write(yxml.toString)
          }
      }
    case _ =>
  }
}

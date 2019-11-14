package de.qaware.dumpimporter.steps.pide

object PideField extends Enumeration {
  final val Accepted = Value("accepted")
  final val Timing = Value("timing")
  final val Running = Value("running")
  final val Finished = Value("finished")
  final val XmlBody = Value("xml_body")
  final val String = Value("string")
  final val NoCompletion = Value("no_completion")
  final val Delimiter = Value("delimiter")
  final val Entity = Value("entity")
  final val Def = Value("def")
  final val Comment = Value("comment")
  final val Delete = Value("delete")
  final val Cartouche = Value("cartouche")
  final val Keyword2 = Value("keyword2")
  final val Where = Value("where")
  final val Kind = Value("kind")
  final val Keyword = Value("keyword")
  final val Constant = Value("constant")
  final val For = Value("for")

  final val DefDelimiter = Value("|")
  final val TypeDelimiter = Value("::")
  final val NameDelimiter = Value(":")
}

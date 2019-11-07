package de.qaware.dumpimporter.dataaccess

import scala.language.postfixOps
import scala.util.matching.Regex

import better.files.File
import com.typesafe.scalalogging.Logger

/** File in the repository.
  *
  * @param relativeName file path relative to repository root
  * @param file the file itself
  */
final case class RepositoryFile(relativeName: String, file: File) {
  override def toString: String = s"~$relativeName"
}

/** Reader for repository files.
  *
  * @param rootdir of the repository
  */
final case class RepositoryReader(rootdir: File) {
  private val logger = Logger[RepositoryReader]

  private def relativeFile(file: File): String = {
    file.canonicalPath.substring(rootdir.canonicalPath.length)
  }

  /** Finds files in repository and reads as string.
    *
    * @param regex to match the file name or relative file path
    * @return an iterator over all found [[RepositoryFile]]s
    */
  def readAll(regex: Regex): Seq[RepositoryFile] = {
    rootdir list { file =>
      file.isReadable && !file.isDirectory && (regex.pattern.matcher(file.name).matches
      || regex.pattern.matcher(relativeFile(file)).matches)
    } map { file =>
      val repoFile = RepositoryFile(relativeFile(file), file)
      logger.debug("Found file {} in {}", repoFile, rootdir)
      repoFile
    } toSeq
  }
}

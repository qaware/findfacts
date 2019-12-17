import sbt._

object Resolvers {
  val restlet = "Restlet" at "https://maven.restlet.com/"
  val sonatypeRelease = Resolver.sonatypeRepo("releases")
  val sonatypeSnapshot = Resolver.sonatypeRepo("snapshots")
  val local = Resolver.mavenLocal

  val all = Seq(restlet, sonatypeRelease, sonatypeSnapshot, local)
}
import sbt._

object Resolvers {
  val all: Seq[Resolver] = Seq(
    "Restlet" at "https://maven.restlet.com/",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.mavenLocal
  )
}
object Profiles extends Enumeration {
  final val UiProfile = Value("ui")
  final val LoaderProfile = Value("loader")
  final val MemoryProfile = Value("memory")

  private def fromString(s: String): Profiles.Value = {
    values.find(_.toString == s).getOrElse(throw new IllegalArgumentException(s"Profile $s does not exist!"))
  }

  lazy val activeProfiles: Set[Profiles.Value] =
    Option(System.getProperty("profiles")).toSeq.flatMap(_.split(",")).map(fromString).toSet

  def active(profile: Profiles.Value): Boolean = activeProfiles.contains(profile)
}

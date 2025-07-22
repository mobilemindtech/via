val sharedSettings = Seq(
  scalaVersion := "3.7.1",
  scalacOptions ++= Seq(
    "-new-syntax",
    // "-no-indent",
    "-Wvalue-discard",
    "-Wunused:all",
    // "-Werror",
    "-deprecation",
    "-explain",
    "-explain-cyclic",
    "-rewrite",
    "-source:future"
  ),
  javacOptions ++= Seq("-source", "24", "-target", "24")
)

lazy val via =
  // select supported platforms
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Full) // [Pure, Full, Dummy], default: CrossType.Full
    .withoutSuffixFor(JVMPlatform)
    .in(file("via"))
    .settings(sharedSettings *)
    .settings(
      name := "via",
      organization := "io.via",
      version := "0.0.1"
    )
    .jvmSettings(
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
    )
    // configure Scala-Native settings
    .nativeSettings( /* ... */ ) // defined in sbt-scala-native

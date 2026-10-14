organization := "io.via"
scalaVersion := "3.10.0"
version := "0.0.1"
scalacOptions ++= Seq(
  "-new-syntax",
  // "-no-indent",
  "-Wvalue-discard",
  "-Wunused:all",
  "-Werror",
  "-deprecation",
  "-explain",
  "-explain-cyclic",
  "-rewrite",
  "-source:future"
)
javacOptions ++= Seq("-source", "27", "-target", "27")

lazy val via =
  // select supported platforms
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Full) // [Pure, Full, Dummy], default: CrossType.Full
    // .withoutSuffixFor(JVMPlatform)
    .in(file("via"))
    .settings(name := "via")
    // .settings(sharedSettings *)
    .jvmSettings(
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % "test"
    )
    // configure Scala-Native settings
    .nativeSettings( /* ... */ ) // defined in sbt-scala-native

lazy val root = (project in file("."))
  .aggregate(via.jvm, via.js, via.native)
  .settings(
    name := "via-root",
    publish / skip := true
  )

publishLocalConfiguration ~= { _.withOverwrite(true) }

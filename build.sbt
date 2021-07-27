lazy val root = project
  .in(file("."))
  .settings(
    name := "recursion-schemes",
    scalaVersion := "3.0.1",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.27" % Test
  )

name := "Cthulhu Wars Solo HRF"

version := "1.18"

scalaVersion := "2.13.16"

scalacOptions := Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-Xlint:infer-any",
    "-Wconf:" + List(
        "will become a keyword",
        "procedure syntax",
        "match may not be exhaustive",
    ).map("msg=" + _ + ":s").mkString(",")
)

Compile / unmanagedSourceDirectories += baseDirectory.value / "base"

libraryDependencies += "com.lihaoyi" %%% "fastparse" % "3.0.2"

libraryDependencies += "com.lihaoyi" %%% "pprint" % "0.7.0"

libraryDependencies += "com.lihaoyi" %%% "fansi" % "0.4.0"

Compile / sourceGenerators += Def.task {
    val file = (Compile / sourceManaged).value / "info.scala"
    IO.write(file, """package hrf { object BuildInfo { val name = "%s" ; val version = "%s" ; val time = %d ; val seed = "%s" } }""".stripMargin.format(name.value, version.value, System.currentTimeMillis % (24 * 60 * 60 * 1000), scala.util.Random.alphanumeric.take(16)))
    Seq(file)
}

bspEnabled := false

maxErrors := 5

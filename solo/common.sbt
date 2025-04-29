name := "Cthulhu Wars Solo HRF"

version := "1.12"

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

libraryDependencies += "com.lihaoyi" %%% "fastparse" % "3.0.2"

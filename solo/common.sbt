name := "Cthulhu Wars Solo HRF"

version := "1.9"

scalaVersion := "2.13.15"

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
    //     "a number and a",
    //     "outer reference in this type test",
        "match may not be exhaustive",
    //     "unreachable code",
    //     "analysis reached max recursion depth",
    //     "definition should have explicit type"
    ).map("msg=" + _ + ":s").mkString(",")
)

libraryDependencies += "com.lihaoyi" %%% "fastparse" % "3.0.2"

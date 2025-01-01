name := "Cthulhu Wars Online"

version := "8.0"

scalaVersion := "2.13.15"

scalacOptions := Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:postfixOps",
    "-Xlint:infer-any",
    "-Wconf:" + List(
        "will become a keyword",
        "procedure syntax",
        "a number and a",
        "implicitConversions visible",
        "outer reference in this type test",
        "match may not be exhaustive",
        "unreachable code",
        "analysis reached max recursion depth",
        "definition should have explicit type"
    ).map("msg=" + _ + ":s").mkString(",")
)

libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.4.0"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.7.0"
libraryDependencies += "ch.megard" %% "akka-http-cors" % "1.2.0"

libraryDependencies += "com.typesafe.slick" %% "slick" % "3.5.2"
libraryDependencies += "com.typesafe.slick" %% "slick-hikaricp" % "3.5.2"

libraryDependencies += "org.hsqldb" % "hsqldb" % "2.7.4"

libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.16"

run / fork := true

Global / cancelable := true

trapExit := false

assembly / assemblyMergeStrategy := {
    case x if x.endsWith("module-info.class") => MergeStrategy.discard
    case x => (assembly / assemblyMergeStrategy).value(x)
}

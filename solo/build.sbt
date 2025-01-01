enablePlugins(ScalaJSPlugin)

Compile / mainClass := Some("cws.CthulhuWarsSolo")

unmanagedSources / excludeFilter := "Stats.scala" || "ReflectStub.scala" || "Host.scala"

scalaJSUseMainModuleInitializer := true

Compile / sourceGenerators += Def.task {
    val file = (Compile / sourceManaged).value / "info.scala"
    IO.write(file, """package cws { object Info { val name = "%s"; val version = "%s"; val time = %d } }""".stripMargin.format(name.value, version.value, System.currentTimeMillis % (24 * 60 * 60 * 1000)))
    Seq(file)
}

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"

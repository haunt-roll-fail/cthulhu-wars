enablePlugins(ScalaJSPlugin)

Compile / mainClass := Some("cws.CthulhuWarsSolo")

unmanagedSources / excludeFilter := "Stats.scala" || "ReflectStub.scala" || "Host.scala"

scalaJSUseMainModuleInitializer := true

scalaJSLinkerConfig ~= { _.withOptimizer(false) }

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0"

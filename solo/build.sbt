enablePlugins(ScalaJSPlugin)

Compile / mainClass := Some("cws.CthulhuWarsSolo")

unmanagedSources / excludeFilter := "Stats.scala" || "ReflectStub.scala" || "Host.scala"

scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xexperimental",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint",
  "-Ybackend:GenBCode",
  "-Ydelambdafy:method",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Yrangepos",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

scalacOptions in (Compile, console) --= Seq(
  "-Yno-imports",
  "-Ywarn-unused-import"
)

libraryDependencies += "org.specs2" %% "specs2-core" % "3.6.4" % "test"

wartremoverErrors ++= Warts.allBut(Wart.DefaultArguments, Wart.Throw)

// coverageEnabled := true
// sbt ";test ;coverageReport"
// chromium target/scala-2.11/scoverage-report/index.html
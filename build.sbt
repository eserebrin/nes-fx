name := "nes-fx"
version := "1.0"
scalaVersion := "2.12.10"
run / fork := true
run / connectInput := true

libraryDependencies ++= Seq(
	"org.scalafx" %% "scalafx" % "8.0.144-R12",
	// "com.novocode" % "junit-interface" % "0.11" % Test,
	// "org.scalactic" %% "scalactic" % "3.0.5",
	// "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

mainClass in (Compile, run) := Some("nes.emu.Console")

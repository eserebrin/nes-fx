package nes.emu

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.animation.AnimationTimer
import java.io.PrintWriter
import java.io.File
import scalafx.scene.input.KeyEvent
import scalafx.scene.input.KeyCode

object Console extends JFXApp {

    // TODO: Variable display zoom
    val DisplayWidth = 256 * 2
    val DisplayHeight = 240 * 2

    // Emulator Info File logging setup
    val FileLogPrintWriter = new PrintWriter(new File("log.txt"))
    var fileLog = ""

    // Create Emulator Components
    val memory = new Memory
    val cpu = new CPU(memory)

    // Create window and run main loop
    stage = new JFXApp.PrimaryStage {
        title = "NES Emulator"
        scene = new Scene(DisplayHeight, DisplayHeight) {
            val canvas = new Canvas(DisplayWidth, DisplayHeight)
            content = canvas
            val g = canvas.graphicsContext2D

            // TODO: Controller handling
            canvas.onKeyPressed = (e: KeyEvent) => e.code match {

                // Power on / off
                // TODO: power on
                case KeyCode.P => {
                    FileLogPrintWriter.write(fileLog)
                    FileLogPrintWriter.close()
                    sys.exit()
                }

                case _ =>
            }

            // Main loop
            private var oldT = 0L
            val MainLoop = AnimationTimer(t => {
                
                // regulate to 60 fps
                if (t - oldT > 1e9 / 60) {
                // if (io.StdIn.readLine() == "") {
                    oldT = t

                    cpu.executeCycle()
                    fileLog += cpu.createTextLogOutput()
                }

            })

            MainLoop.start()
            canvas.requestFocus()

        }
    } 

}
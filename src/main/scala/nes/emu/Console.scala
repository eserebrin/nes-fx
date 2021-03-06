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

    private val MasterClockSpeed = 21477272 // Hz

    private var fileLog = ""

    def addLog(log: String): Unit = fileLog += "   " + log + "\n"

    def createLogFileAndExit(): Unit = {
        val FileLogPrintWriter = new PrintWriter(new File("log.txt"))
        FileLogPrintWriter.write(fileLog)
        FileLogPrintWriter.close()
        sys.exit()
    }


    // Create window and run main loop
    stage = new JFXApp.PrimaryStage {
        title = "NES Emulator"
        scene = new Scene(DisplayHeight, DisplayHeight) {
            val canvas = new Canvas(DisplayWidth, DisplayHeight)
            content = canvas
            val g = canvas.graphicsContext2D
            val args = parameters.raw

            val RomFileName = args(0)

            // Create Emulator Components
            val mapper = Mapper(new Cartridge(RomFileName))
            val cpu = new CPU(mapper)
            val ppu = new PPU(mapper)


            // TODO: Controller handling
            canvas.onKeyPressed = (e: KeyEvent) => e.code match {

                // P -> Power off
                case KeyCode.P => createLogFileAndExit()

                case _ =>
            }

            // Main loop
            var oldT = 0L
            val MainLoop = AnimationTimer(t => {
                // Regulate main loop to 60 fps
                if (t - oldT > 1e9 / 60) {
                    // Emulate clock speed
                    for (i <- 0 until MasterClockSpeed / 60) {
                        if (i % 12 == 0) {
                            println(cpu.createTextLogOutput())
                            // fileLog += cpu.createSingleLineTextLogOutput()

                            cpu.executeCycle()

                            //NESTEST CYCLE LIMIT:
                            if (cpu.cycleCount >= 26554) createLogFileAndExit()
                        }
                    }
                    oldT = t
                }
            })

            canvas.requestFocus()
            MainLoop.start()
        }
    } 

}
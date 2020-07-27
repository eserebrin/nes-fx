package nes.emu

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.animation.AnimationTimer

object Console extends JFXApp {

    // TODO: Variable display zoom
    val DisplayWidth = 256 * 2
    val DisplayHeight = 240 * 2

    stage = new JFXApp.PrimaryStage {
        title = "NES Emulator"
        scene = new Scene(DisplayHeight, DisplayHeight) {
            val canvas = new Canvas(DisplayWidth, DisplayHeight)
            content = canvas
            val g = canvas.graphicsContext2D

            // Create Emulator Components
            val memory = new Memory
            val cpu = new CPU(memory)

            // Main loop
            private var oldT = 0L
            val loop = AnimationTimer(t => {

                // regulate to 60 fps
                if (t - oldT < 1e9 / 60) {
                    oldT = t

                    cpu.executeCycle()
                }

            })

            loop.start()
            canvas.requestFocus()
        }
    } 

}
package nes.emu

import scala.collection.mutable

class CPU(memory: Memory) {

    // CPU registers
    private var accumulator = 0
    private var xIndex = 0
    private var yIndex = 0
    private var programCounter = 0
    private var stackPointer = 0
    private var status = 0


    private object Cycles {

        // The functions left to carry out for the current operation
        private var cyclesToPerform = mutable.Buffer[() => Unit]()

        // The information needed to complete the current operation
        private var storedData = mutable.Buffer[Int]()

        def add(operation: () => Unit): Unit = cyclesToPerform += operation
        def executeNext(): Unit = cyclesToPerform.remove(0)()
        def isOperationFinished(): Boolean = cyclesToPerform.length == 0

        def storeByte(byte: Int): Unit = storedData += byte
        def getNextStoredByte(): Int = storedData.remove(0)
        def clearStoredData(): Unit = storedData = mutable.Buffer[Int]()

    }


    /* ------ CPU cycle helper methods ------ */

    private def fetchNextByte(): Unit = {
        Cycles.storeByte(memory(programCounter))
        programCounter += 1
    }

    private def fetchNextByteFromAddress(): Unit = {
        val pointerLowByte = Cycles.getNextStoredByte()
        val pointerHighByte = Cycles.getNextStoredByte()
        val pointer = pointerHighByte << 8 | pointerLowByte
        Cycles.storeByte(memory(pointer))
    }

    // private def fetchAddress(): Int = {
    //     Cycles.add(fetchNextByte)
    //     Cycles.add(fetchNextByte)
    //     val lowByte = Cycles.getNextStoredByte()
    //     val highByte = Cycles.getNextStoredByte() << 8
    //     highByte | lowByte
    // }

    /* ------ Main CPU operation methods ------ */

    private def startOperation(opcode: Int): Unit = opcode match {

        // JMP -- Absolute
        case 0x4C => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val addressLowByte = Cycles.getNextStoredByte()
                val addressHighByte = memory(programCounter) << 8
                programCounter = addressHighByte | addressLowByte
            })
        }

        // JMP -- Indirect
        case 0x6C => {
            Cycles.add(fetchNextByte)
            Cycles.add(fetchNextByte)
            // TODO: page boundary bug
            Cycles.add(fetchNextByteFromAddress)
            Cycles.add(() => {
                programCounter = Cycles.getNextStoredByte()
            })
        }

        // LDA -- Immediate
        case 0xA9 => {
            Cycles.add(() => {
                accumulator = memory(programCounter)
                programCounter += 1
            })
        }

        // LDA -- Zero Page
        case 0xA5 => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                accumulator = memory(Cycles.getNextStoredByte())
            })
        }

        // LDA -- Zero Page, X
        case 0xB5 => {
        }

        // NOP -- Implied
        case 0xEA => Cycles.add(return)

        case _ =>
    }

    private def fetchOpcode(): Unit = {
        val opcode = memory(programCounter)
        programCounter += 1
        startOperation(opcode)
    }

    /**
      * Main entry point for the CPU, to be called by the Console
      * each time through the main loop.
      */ 
    def executeCycle(): Unit = {
        println(f"$programCounter%04X: ${memory(programCounter)}%02X")
        if (Cycles.isOperationFinished()) {
            Cycles.clearStoredData()
            fetchOpcode()
        }
        else Cycles.executeNext()
    }

}
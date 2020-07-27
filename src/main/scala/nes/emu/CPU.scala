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
        private var storedData = mutable.Buffer[Byte]()

        def add(operation: () => Unit): Unit = cyclesToPerform += operation
        def executeNext(): Unit = cyclesToPerform.remove(0)()
        def isOperationFinished(): Boolean = cyclesToPerform.length == 0

        def storeByte(byte: Byte): Unit = storedData += byte
        def getNextStoredByte(): Byte = storedData.remove(0)
        def clearStoredData(): Unit = storedData = mutable.Buffer[Byte]()

    }


    /* ------ CPU cycle helper methods ------ */

    private def fetchByte(): Unit = {
        Cycles.storeByte(memory(programCounter))
        programCounter += 1
    }

    // private def fetchAddress(): Int = {
    //     Cycles.add(fetchByte)
    //     Cycles.add(fetchByte)
    //     val lowByte = Cycles.getNextStoredByte()
    //     val highByte = Cycles.getNextStoredByte() << 8
    //     highByte | lowByte
    // }

    /* ------ Main CPU operation methods ------ */

    private def startOperation(opcode: Byte): Unit = opcode match {

        // JMP -- Absolute
        case 0x4C => {
            Cycles.add(fetchByte)
            Cycles.add(() => {
                fetchByte()
                val addressLowByte = Cycles.getNextStoredByte()
                val addressHighByte = Cycles.getNextStoredByte() << 8
                programCounter = addressHighByte | addressLowByte
            })
        }

        // NOP
        case 0xEA => Cycles.add(return)
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
        if (Cycles.isOperationFinished()) {
            Cycles.clearStoredData()
            fetchOpcode()
        }
        else Cycles.executeNext()
    }

}
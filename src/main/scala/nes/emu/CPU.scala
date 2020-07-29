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


    // Flags
    private val ZeroFlagMask = 1 << 1
    private val NegativeFlagMask = 1 << 7

    // Total number of CPU cycles run since power on
    private var cycleCount = 0L


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

    def createTextLogOutput(): String = {
        f"""
            |---------------------------------------
            |              CPU (Cycle ${cycleCount}%d)
            |---------------------------------------
            |=== Registers ===
            |Program Counter: ${programCounter}%04X
            |Data:            ${memory(programCounter)}%02X
            |Accumulator:     ${accumulator}%02X
            |Index X:         ${xIndex}%02X
            |Index Y:         ${yIndex}%02X
            |Stack Pointer:   ${stackPointer}%02X
            |=== Status ===
            |Negative: ${(status & NegativeFlagMask) >> 1}%1d
            |Overflow:
            |Decimal:
            |Interrupt Disable:
            |Zero: ${(status & ZeroFlagMask) >> 7}%1d
            |Carry:
         """.stripMargin('|')
    }


    /* ------ CPU cycle helper methods ------ */

    private def fetchNextByte(): Unit = {
        Cycles.storeByte(memory(programCounter))
        programCounter += 1
    }

    private def fetchNextByteFromAddress(): Unit = {
        val PointerLowByte = Cycles.getNextStoredByte()
        var pointerHighByte = Cycles.getNextStoredByte()

        // Replicate page boundary bug:
        if ((pointerHighByte & 0xFF) == 0xFF) pointerHighByte = 0
        else pointerHighByte <<= 8

        val Pointer = pointerHighByte | PointerLowByte
        Cycles.storeByte(memory(Pointer))
    }


    /* ------ Flag updating methods ------ */

    private def updateZeroFlag(register: Int): Unit = {
        if (register == 0) status |= ZeroFlagMask
        else status |= ~ZeroFlagMask
    }

    private def updateNegativeFlag(register: Int): Unit = {
        val NewNegativeFlag = register & NegativeFlagMask
        status |= NewNegativeFlag
    }

    // private def fetchAddress(): Int = {
    //     Cycles.add(fetchNextByte)
    //     Cycles.add(fetchNextByte)
    //     val lowByte = Cycles.getNextStoredByte()
    //     val highByte = Cycles.getNextStoredByte() << 8
    //     highByte | lowByte
    // }


    /* ------ Main CPU operation methods ------ */

    /**
      * Main entry point for the CPU, to be called by the Console
      * each time through the main loop.
      */ 
    def executeCycle(): Unit = {
        cycleCount += 1
        if (Cycles.isOperationFinished()) {
            Cycles.clearStoredData()
            fetchOpcode()
        }
        else Cycles.executeNext()
    }

    private def fetchOpcode(): Unit = {
        val Opcode = memory(programCounter)
        programCounter += 1
        startOperation(Opcode)
    }

    private def startOperation(opcode: Int): Unit = opcode match {

        // JMP -- Absolute
        case 0x4C => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = memory(programCounter) << 8
                programCounter = AddressHighByte | AddressLowByte
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
                updateZeroFlag(accumulator)
                updateNegativeFlag(accumulator)
            })
        }

        // LDA -- Zero Page
        case 0xA5 => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                accumulator = memory(Cycles.getNextStoredByte())
                updateZeroFlag(accumulator)
                updateNegativeFlag(accumulator)
            })
        }

        // LDA -- Zero Page, X
        case 0xB5 => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val Address = (Cycles.getNextStoredByte() + xIndex) & 0xFF
                Cycles.storeByte(Address)
            })
            Cycles.add(() => {
                accumulator = Cycles.getNextStoredByte()
                updateZeroFlag(accumulator)
                updateNegativeFlag(accumulator)
            })
        }

        // NOP -- Implied
        case 0xEA => Cycles.add(return)

        case _ =>
    }

}
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
    private val CarryFlagMask = 1
    private val ZeroFlagMask = 1 << 1
    private val InterruptDisableFlagMask = 1 << 2
    private val DecimalFlagMask = 1 << 3
    private val OverflowFlagMask = 1 << 6
    private val NegativeFlagMask = 1 << 7

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
        def peekNextStoredByte(): Int = storedData(0)

    }

    // Total number of CPU cycles run since power on
    private var cycleCount = 0L


    /* ------ Main CPU operation methods ------ */

    /**
      * Main entry point for the CPU, to be called by the Console
      * each time through the main loop.
      */ 
    def executeCycle(): Unit = {
        if (Cycles.isOperationFinished()) fetchOpcode()
        else Cycles.executeNext()
        cycleCount += 1
    }

    private def fetchOpcode(): Unit = {
        val Opcode = memory(programCounter)
        programCounter += 1
        startOperation(Opcode)
    }

    def createTextLogOutput(): String = {
        f"""
            |----------------------
            | CPU (Cycle ${cycleCount}%d)
            |----------------------
            |  === Registers ===
            |Program Counter: ${programCounter}%04X
            |Data:            ${memory(programCounter)}%02X
            |Accumulator:     ${accumulator}%02X
            |Index X:         ${xIndex}%02X
            |Index Y:         ${yIndex}%02X
            |Stack Pointer:   ${stackPointer}%02X
            |  === Status ===
            |Negative: ${(status & NegativeFlagMask) >> 7}%1d
            |Overflow: ${(status & OverflowFlagMask) >> 6}%1d
            |Decimal: ${(status & DecimalFlagMask) >> 3}%1d
            |Interrupt Disable: ${(status & InterruptDisableFlagMask) >> 2}%1d
            |Zero: ${(status & ZeroFlagMask) >> 1}%1d
            |Carry: ${status & CarryFlagMask}%1d
            |----------------------
         """.stripMargin('|')
    }

    private def startOperation(opcode: Int): Unit = opcode match {

        // ADC (Add with carry)
        case 0x69 => createOperationImmediate(adc)
        case 0x65 => createOperationZeroPage(adc)
        case 0x75 => createOperationZeroPageIndexed(adc, xIndex)
        case 0x6D => createOperationAbsolute(adc)
        case 0x7D => createOperationAbsoluteIndexed(adc, xIndex)
        case 0x79 => createOperationAbsoluteIndexed(adc, yIndex)
        case 0x61 => createOperationIndexedIndirect(adc)
        case 0x71 => createOperationIndirectIndexed(adc)

        // AND (Logical AND)
        case 0x29 => createOperationImmediate(and)
        case 0x25 => createOperationZeroPage(and)
        case 0x35 => createOperationZeroPageIndexed(and, xIndex)
        case 0x2D => createOperationAbsolute(and)
        case 0x3D => createOperationAbsoluteIndexed(and, xIndex)
        case 0x39 => createOperationAbsoluteIndexed(and, yIndex)
        case 0x21 => createOperationIndexedIndirect(and)
        case 0x31 => createOperationIndirectIndexed(and)

        // TODO
        // TODO: Read-Modify-Write operation creation
        // TODO
        // ASL (Arithmetic Shift Left)
        case 0x0A => createOperationAccumulator(asl)
        case 0x06 => createOperationZeroPage(asl)
        case 0x16 => createOperationZeroPageIndexed(asl, xIndex)
        case 0x0E => createOperationAbsolute(asl)
        case 0x1E => createOperationAbsoluteIndexed(asl, xIndex)

        // JMP (Jump)
        case 0x4C => createOperationAbsolute3Cycle(jmp)
        case 0x6C => createOperationIndirect(jmp)

        // LDA (Load accumulator)
        case 0xA9 => createOperationImmediate(lda)
        case 0xA5 => createOperationZeroPage(lda)
        case 0xB5 => createOperationZeroPageIndexed(lda, xIndex)
        case 0xAD => createOperationAbsolute(lda)
        case 0xBD => createOperationAbsoluteIndexed(lda, xIndex)
        case 0xB9 => createOperationAbsoluteIndexed(lda, yIndex)
        case 0xA1 => createOperationIndexedIndirect(lda)
        case 0xB1 => createOperationIndirectIndexed(lda)

        // LDA (Load X register)
        case 0xA2 => createOperationImmediate(ldx)
        case 0xA6 => createOperationZeroPage(ldx)
        case 0xB6 => createOperationZeroPageIndexed(ldx, yIndex)
        case 0xAE => createOperationAbsolute(ldx)
        case 0xBE => createOperationAbsoluteIndexed(ldx, yIndex)

        // LDA (Load Y register)
        case 0xA0 => createOperationImmediate(ldy)
        case 0xA4 => createOperationZeroPage(ldy)
        case 0xB4 => createOperationZeroPageIndexed(ldy, xIndex)
        case 0xAC => createOperationAbsolute(ldy)
        case 0xBC => createOperationAbsoluteIndexed(ldy, xIndex)

        // NOP (No operation)
        case 0xEA => createOperationImplied(nop)

        case _ =>

    }


    /* ------ CPU cycle helper methods ------ */

    private def fetchNextByte(): Unit = {
        Cycles.storeByte(memory(programCounter))
        programCounter += 1
    }

    private def getStoredAddress(): Int = {
        val AddressLowByte = Cycles.getNextStoredByte()
        val AddressHighByte = Cycles.getNextStoredByte()
        (AddressHighByte << 8) | AddressLowByte
    }


    /* ------ Flag updating methods ------ */

    // TODO: look into beter flag masking options

    private def updateZeroFlag(register: Int): Unit = {
        if (register == 0) status |= ZeroFlagMask
        else status &= 0xFD // 11111101
    }

    private def updateNegativeFlag(register: Int): Unit = {
        val NewNegativeFlag = register & NegativeFlagMask
        status |= NewNegativeFlag
    }

    // private def updateCarryFlag(register: Int): Unit = {
    //     val NewCarryFlag = (register & 0x100) >> 8
    //     register &= 0xFF
    //     status |= NewCarryFlag
    // }


    /* ------ Operation creation methods ------ */

    private def createOperationImplied(operation: () => Unit): Unit = Cycles.add(operation)

    private def createOperationAccumulator(operation: () => Unit): Unit = Cycles.add(operation)

    private def createOperationImmediate(operation: Int => Unit): Unit = {
        Cycles.add(() => {
            operation(memory(programCounter))
            programCounter += 1
        })
    }

    private def createOperationZeroPage(operation: Int => Unit): Unit = {
        Cycles.add(fetchNextByte)
        Cycles.add(() => operation(memory(Cycles.getNextStoredByte())))
    }

    private def createOperationZeroPageIndexed(operation: Int => Unit, index: Int): Unit = {
        Cycles.add(fetchNextByte)
        Cycles.add(() => {
            val Address = (Cycles.getNextStoredByte() + index) & 0xFF
            Cycles.storeByte(Address)
        })
        Cycles.add(() => operation(memory(Cycles.getNextStoredByte())))
    }

    private def createOperationAbsolute(operation: Int => Unit): Unit = {
        Cycles.add(fetchNextByte)
        Cycles.add(fetchNextByte)
        Cycles.add(() => operation(getStoredAddress()))
    }

    private def createOperationAbsolute3Cycle(operation: Int => Unit): Unit = {
        Cycles.add(fetchNextByte)
        Cycles.add(() => {
            val AddressLowByte = Cycles.getNextStoredByte()
            val AddressHighByte = memory(programCounter) << 8
            operation(AddressHighByte | AddressLowByte)
        })
    }

    private def createOperationAbsoluteIndexed(operation: Int => Unit, index: Int): Unit = {
        Cycles.add(fetchNextByte)
        Cycles.add(fetchNextByte)
        Cycles.add(() => {
            val AddressLowByte = Cycles.getNextStoredByte()
            val AddressHighByte = Cycles.getNextStoredByte() << 8
            val Address = (AddressHighByte | AddressLowByte) + index

            // Extra cycle needed if page boundary crossed
            if (AddressLowByte + index < 0x100) operation(memory(Address))
            else Cycles.add(() => operation(memory(Address)))
        })
    }

    private def createOperationIndirect(operation: Int => Unit): Unit = {
        Cycles.add(fetchNextByte)
        Cycles.add(fetchNextByte)
        Cycles.add(() => {
            val PointerLowByte = Cycles.getNextStoredByte()
            var pointerHighByte = Cycles.getNextStoredByte()

            // Replicate page boundary bug:
            if ((pointerHighByte & 0xFF) == 0xFF) pointerHighByte = 0
            else pointerHighByte <<= 8

            val Pointer = pointerHighByte | PointerLowByte
            Cycles.storeByte(memory(Pointer))
        })
        Cycles.add(() => programCounter = Cycles.getNextStoredByte())
    }

    private def createOperationIndexedIndirect(operation: Int => Unit): Unit = {
        Cycles.add(fetchNextByte)
        Cycles.add(() => {
            Cycles.storeByte((Cycles.getNextStoredByte() + xIndex) & 0xFF)
        })
        Cycles.add(() => {
            val Pointer = Cycles.peekNextStoredByte()
            Cycles.storeByte(memory(Pointer))
        })
        Cycles.add(() => {
            val Pointer = Cycles.getNextStoredByte()
            Cycles.storeByte(memory(Pointer + 1))
        })
        Cycles.add(() => operation(memory(getStoredAddress())))
    }

    private def createOperationIndirectIndexed(operation: Int => Unit): Unit = {
        Cycles.add(fetchNextByte)
        Cycles.add(() => {
            val Pointer = Cycles.peekNextStoredByte()
            Cycles.storeByte(memory(Pointer))
        })
        Cycles.add(() => {
            val Pointer = Cycles.getNextStoredByte()
            Cycles.storeByte(memory(Pointer + 1))
        })
        Cycles.add(() => {
            val AddressLowByte = Cycles.getNextStoredByte()
            val AddressHighByte = Cycles.getNextStoredByte() << 8
            val Address = (AddressHighByte | AddressLowByte) + yIndex

            // Extra cycle needed if page boundary crossed
            if (AddressLowByte + yIndex < 0x100) operation(memory(Address))
            else Cycles.add(() => operation(memory(Address)))
        })
    }


    /* ------- Opcodes ------- */

    private def adc(data: Int): Unit = {
        val PreviousSignBit = accumulator & NegativeFlagMask

        accumulator += data
        accumulator += status & CarryFlagMask

        // Update Carry flag
        val NewCarryFlag = (accumulator & 0x100) >> 8
        accumulator &= 0xFF
        status |= NewCarryFlag

        // Update Overflow flag
        if ((accumulator & NegativeFlagMask) != PreviousSignBit) status |= OverflowFlagMask
        else status &= 0xBF // 10111111

        updateZeroFlag(accumulator)
        updateNegativeFlag(accumulator)
    }

    private def and(data: Int): Unit = {
        accumulator &= data
        updateZeroFlag(accumulator)
        updateNegativeFlag(accumulator)
    }

    // Overloaded no argument version (Accumulator)
    private def asl(): Unit = {
        // Set Carry flag to previous bit 7
        status |= ((accumulator & NegativeFlagMask) >> 7)

        accumulator <<= 1

        updateZeroFlag(accumulator)
        updateNegativeFlag(accumulator)
    }

    private def asl(data: Int): Unit = {
        // Set Carry flag to previous bit 7
        status |= ((memory(data) & NegativeFlagMask) >> 7)

        memory(data) <<= 1

        updateZeroFlag(memory(data))
        updateNegativeFlag(memory(data))
    }

    private def jmp(data: Int): Unit = programCounter = data

    private def lda(data: Int): Unit = {
        accumulator = data
        updateZeroFlag(accumulator)
        updateNegativeFlag(accumulator)
    }

    private def ldx(data: Int): Unit = {
        xIndex = data
        updateZeroFlag(xIndex)
        updateNegativeFlag(xIndex)
    }

    private def ldy(data: Int): Unit = {
        yIndex = data
        updateZeroFlag(yIndex)
        updateNegativeFlag(yIndex)
    }

    private def nop(): Unit = {}


}
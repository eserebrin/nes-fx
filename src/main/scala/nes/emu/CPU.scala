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
        def peekNextStoredByte(): Int = storedData(0)

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


    /* ------ CPU cycle helper methods ------ */

    private def fetchNextByte(): Unit = {
        Cycles.storeByte(memory(programCounter))
        programCounter += 1
    }

    private def fetchByteIndirect(): Unit = {
        val PointerLowByte = Cycles.getNextStoredByte()
        var pointerHighByte = Cycles.getNextStoredByte()

        // Replicate page boundary bug:
        if ((pointerHighByte & 0xFF) == 0xFF) pointerHighByte = 0
        else pointerHighByte <<= 8

        val Pointer = pointerHighByte | PointerLowByte
        Cycles.storeByte(memory(Pointer))
    }

    private def createOperation(
        addressingMode: String,
        uniquePart: Int => Unit,
        flagUpdates: Array[() => Unit]
    ): Unit = addressingMode match {

        case "immediate" => {
            Cycles.add(() => {
                uniquePart(memory(programCounter))
                programCounter += 1
                flagUpdates.foreach(_())
            })
        }

        case "zero_page" => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                uniquePart(memory(Cycles.getNextStoredByte()))
                flagUpdates.foreach(_())
            })
        }

        case "zero_page_x" => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val Address = (Cycles.getNextStoredByte() + xIndex) & 0xFF
                Cycles.storeByte(Address)
            })
            Cycles.add(() => {
                uniquePart(memory(Cycles.getNextStoredByte()))
                flagUpdates.foreach(_())
            })
        }

        case "absolute" => {
            Cycles.add(fetchNextByte)
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = Cycles.getNextStoredByte() << 8
                uniquePart(AddressHighByte | AddressLowByte)
                flagUpdates.foreach(_())
            })
        }

        case "absolute_x" | "absolute_y" => {

            // Determine which index to add
            val Index = if (addressingMode == "absolute_x") xIndex else yIndex

            Cycles.add(fetchNextByte)
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = Cycles.getNextStoredByte() << 8
                val Address = (AddressHighByte | AddressLowByte) + Index

                // Extra cycle needed if page boundary crossed
                if (AddressLowByte + xIndex < 0x100) {
                    uniquePart(memory(Address))
                    flagUpdates.foreach(_())
                }
                else Cycles.add(() => {
                    uniquePart(memory(Address))
                    flagUpdates.foreach(_())
                })

            })
        }

        case "indirect_x" => {
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
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = Cycles.getNextStoredByte()
                val Address = (AddressHighByte << 8) | AddressLowByte
                uniquePart(memory(Address))
                flagUpdates.foreach(_())
            })
        }

        case "indirect_y" => {
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
                if (AddressLowByte + yIndex < 0x100) {
                    uniquePart(memory(Address))
                    flagUpdates.foreach(_())
                }
                else Cycles.add(() => {
                    uniquePart(memory(Address))
                    flagUpdates.foreach(_())
                })
            })
        }

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

    private def updateCarryFlag(register: Int): Unit = {
        ???
    }


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


    // WARNING!!! Opcodes ahead
    private def startOperation(opcode: Int): Unit = opcode match {


        /* ------- JMP ------- */

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
            Cycles.add(fetchByteIndirect)
            Cycles.add(() => {
                programCounter = Cycles.getNextStoredByte()
            })
        }


        /* ------- LDA (Load accumulator) ------- */
        case 0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => createOperation(

            // Addressing mode
            opcode match {
                case 0xA9 => "immediate"
                case 0xA5 => "zero_page"
                case 0xB5 => "zero_page_x"
                case 0xAD => "absolute"
                case 0xBD => "absolute_x"
                case 0xB9 => "absolute_y"
                case 0xA1 => "indirect_x"
                case 0xB1 => "indirect_y"
            },

            // Unique operation
            data => accumulator = data,

            // Flag updates
            Array(
                () => updateZeroFlag(accumulator),
                () => updateNegativeFlag(accumulator)
            )

        )


        /* ------- LDX (Load X register) ------- */
        case 0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => createOperation(

            // Addressing mode
            opcode match {
                case 0xA2 => "immediate"
                case 0xA6 => "zero_page"
                case 0xB6 => "zero_page_y"
                case 0xAE => "absolute"
                case 0xBE => "absolute_y"
            },

            // Unique operation
            data => xIndex = data,

            // Flag updates
            Array(
                () => updateZeroFlag(xIndex),
                () => updateNegativeFlag(xIndex)
            )

        )


        /* ------- LDY (Load Y register) ------- */
        case 0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => createOperation(

            // Addressing mode
            opcode match {
                case 0xA0 => "immediate"
                case 0xA4 => "zero_page"
                case 0xB4 => "zero_page_x"
                case 0xAC => "absolute"
                case 0xBC => "absolute_x"
            },

            // Unique operation
            data => yIndex = data,

            // Flag updates
            Array(
                () => updateZeroFlag(yIndex),
                () => updateNegativeFlag(yIndex)
            )

        )


        // NOP -- Implied
        case 0xEA => Cycles.add(return)

        case _ =>
    }

}
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
        def clearStoredData(): Unit = storedData = mutable.Buffer[Int]()

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


        /* ------- LDA ------- */

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
                accumulator = memory(Cycles.getNextStoredByte())
                updateZeroFlag(accumulator)
                updateNegativeFlag(accumulator)
            })
        }

        // LDA -- Absolute
        case 0xAD => {
            Cycles.add(fetchNextByte)
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = Cycles.getNextStoredByte() << 8
                accumulator = AddressHighByte | AddressLowByte
                updateZeroFlag(accumulator)
                updateNegativeFlag(accumulator)
            })
        }

        // LDA -- Abslolute, X
        case 0xBD => {
            Cycles.add(fetchNextByte)
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = Cycles.getNextStoredByte() << 8
                val Address = (AddressHighByte | AddressLowByte) + xIndex

                // Extra cycle needed if page boundary crossed
                if (AddressLowByte + xIndex < 0x100) {
                    accumulator = memory(Address)
                    updateZeroFlag(accumulator)
                    updateNegativeFlag(accumulator)
                }
                else Cycles.add(() => {
                    accumulator = memory(Address)
                    updateZeroFlag(accumulator)
                    updateNegativeFlag(accumulator)
                })

            })
        }

        // LDA -- Abslolute, Y
        case 0xB9 => {
            Cycles.add(fetchNextByte)
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = Cycles.getNextStoredByte() << 8
                val Address = (AddressHighByte | AddressLowByte) + yIndex

                // Extra cycle needed if page boundary crossed
                if (AddressLowByte + yIndex < 0x100) {
                    accumulator = memory(Address)
                    updateZeroFlag(accumulator)
                    updateNegativeFlag(accumulator)
                }
                else Cycles.add(() => {
                    accumulator = memory(Address)
                    updateZeroFlag(accumulator)
                    updateNegativeFlag(accumulator)
                })

            })
        }

        // LDA -- (Indirect, X)
        case 0xA1 => {
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
                val AddressHighByte = Cycles.getNextStoredByte() << 8
                accumulator = memory(AddressHighByte | AddressLowByte)
                updateZeroFlag(accumulator)
                updateNegativeFlag(accumulator)
            })
        }

        // LDA -- (Indirect), Y
        case 0xB1 => {
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
                    accumulator = memory(Address)
                    updateZeroFlag(accumulator)
                    updateNegativeFlag(accumulator)
                }
                else Cycles.add(() => {
                    accumulator = memory(Address)
                    updateZeroFlag(accumulator)
                    updateNegativeFlag(accumulator)
                })
            })
        }


        /* ------- LDX ------- */

        // LDX -- Immediate
        case 0xA2 => {
            Cycles.add(() => {
                xIndex = memory(programCounter)
                programCounter += 1
                updateZeroFlag(xIndex)
                updateNegativeFlag(xIndex)
            })
        }

        // LDX -- Zero Page
        case 0xA6 => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                xIndex = memory(Cycles.getNextStoredByte())
                updateZeroFlag(xIndex)
                updateNegativeFlag(xIndex)
            })
        }

        // LDX -- Zero Page, Y
        case 0xB6 => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val Address = (Cycles.getNextStoredByte() + yIndex) & 0xFF
                Cycles.storeByte(Address)
            })
            Cycles.add(() => {
                xIndex = memory(Cycles.getNextStoredByte())
                updateZeroFlag(xIndex)
                updateNegativeFlag(xIndex)
            })
        }

        // LDX -- Absolute
        case 0xAE => {
            Cycles.add(fetchNextByte)
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = Cycles.getNextStoredByte() << 8
                xIndex = AddressHighByte | AddressLowByte
                updateZeroFlag(xIndex)
                updateNegativeFlag(yIndex)
            })
        }

        // LDX -- Abslolute, Y
        case 0xBE => {
            Cycles.add(fetchNextByte)
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = Cycles.getNextStoredByte() << 8
                val Address = (AddressHighByte | AddressLowByte) + yIndex

                // Extra cycle needed if page boundary crossed
                if (AddressLowByte + yIndex < 0x100) {
                    xIndex = memory(Address)
                    updateZeroFlag(xIndex)
                    updateNegativeFlag(xIndex)
                }
                else Cycles.add(() => {
                    xIndex = memory(Address)
                    updateZeroFlag(xIndex)
                    updateNegativeFlag(xIndex)
                })

            })
        }


        /* ------- LDY ------- */

        // LDY -- Immediate
        case 0xA0 => {
            Cycles.add(() => {
                yIndex = memory(programCounter)
                programCounter += 1
                updateZeroFlag(yIndex)
                updateNegativeFlag(yIndex)
            })
        }

        // LDY -- Zero Page
        case 0xA4 => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                yIndex = memory(Cycles.getNextStoredByte())
                updateZeroFlag(yIndex)
                updateNegativeFlag(yIndex)
            })
        }

        // LDY -- Zero Page, X
        case 0xB4 => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val Address = (Cycles.getNextStoredByte() + xIndex) & 0xFF
                Cycles.storeByte(Address)
            })
            Cycles.add(() => {
                yIndex = memory(Cycles.getNextStoredByte())
                updateZeroFlag(yIndex)
                updateNegativeFlag(yIndex)
            })
        }

        // LDY -- Absolute
        case 0xAC => {
            Cycles.add(fetchNextByte)
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = Cycles.getNextStoredByte() << 8
                yIndex = AddressHighByte | AddressLowByte
                updateZeroFlag(yIndex)
                updateNegativeFlag(yIndex)
            })
        }

        // LDY -- Abslolute, X
        case 0xBC => {
            Cycles.add(fetchNextByte)
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = Cycles.getNextStoredByte() << 8
                val Address = (AddressHighByte | AddressLowByte) + xIndex

                // Extra cycle needed if page boundary crossed
                if (AddressLowByte + xIndex < 0x100) {
                    yIndex = memory(Address)
                    updateZeroFlag(yIndex)
                    updateNegativeFlag(yIndex)
                }
                else Cycles.add(() => {
                    yIndex = memory(Address)
                    updateZeroFlag(yIndex)
                    updateNegativeFlag(yIndex)
                })

            })
        }


        // NOP -- Implied
        case 0xEA => Cycles.add(return)

        case _ =>
    }

}
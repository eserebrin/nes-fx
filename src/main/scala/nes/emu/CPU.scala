package nes.emu

import scala.collection.mutable
import scalafx.collections.ObservableBuffer.Add

class CPU(memory: Memory) {

    // CPU registers
    private var accumulator = 0
    private var xIndex = 0
    private var yIndex = 0
    private var programCounter = 0
    private var stackPointer = 0xFD
    private var status = 0x34

    // Status Flags
    private val CarryFlagMask = 1
    private val ZeroFlagMask = 1 << 1
    private val InterruptDisableFlagMask = 1 << 2
    private val DecimalFlagMask = 1 << 3
    private val BFlagMask = 1 << 4
    private val OverflowFlagMask = 1 << 6
    private val NegativeFlagMask = 1 << 7

    private object Cycles {

        // The functions left to carry out for the current operation
        private var cyclesToPerform = mutable.Buffer[() => Unit]()

        // The information needed to complete the current operation
        private var storedData = mutable.Buffer[Int]()

        def add(operation: () => Unit = () => {}): Unit = cyclesToPerform += operation
        def executeNext(): Unit = cyclesToPerform.remove(0)()
        def isOperationFinished(): Boolean = cyclesToPerform.length == 0

        def storeByte(byte: Int): Unit = storedData += byte
        def getNextStoredByte(): Int = storedData.remove(0)
        def peekNextStoredByte(): Int = storedData(0)

    }

    // Total number of CPU cycles run since power on
    private var cycleCount = 0L

    private object OperationType extends Enumeration {
        type T = Value
        val Read, ReadModifyWrite, Write, Other = Value
    }

    private object AddressingMode extends Enumeration {
        type T = Value
        val Implied = Value
        val Accumulator = Value
        val Relative = Value
        val Immediate = Value
        val ZeroPage = Value
        val ZeroPageIndexed = Value
        val Absolute = Value
        val AbsoluteIndexed = Value
        val Indirect = Value
        val IndexedIndirect = Value
        val IndirectIndexed = Value
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
            |  === Status Flags ===
            |Negative: ${(status & NegativeFlagMask) >> 7}%1d
            |Overflow: ${(status & OverflowFlagMask) >> 6}%1d
            |Decimal: ${(status & DecimalFlagMask) >> 3}%1d
            |Break Command (B): ${(status & BFlagMask) >> 4}%1d
            |Interrupt Disable: ${(status & InterruptDisableFlagMask) >> 2}%1d
            |Zero: ${(status & ZeroFlagMask) >> 1}%1d
            |Carry: ${status & CarryFlagMask}%1d
            |----------------------
         """.stripMargin('|')
    }

    private def startOperation(opcode: Int): Unit = opcode match {

        // ADC (Add with carry)
        case 0x69 => createOperation(AddressingMode.Immediate, OperationType.Read, adc)
        case 0x65 => createOperation(AddressingMode.ZeroPage, OperationType.Read, adc)
        case 0x75 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Read, adc, xIndex)
        case 0x6D => createOperation(AddressingMode.Absolute, OperationType.Read, adc)
        case 0x7D => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, adc, xIndex)
        case 0x79 => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, adc, yIndex)
        case 0x61 => createOperation(AddressingMode.IndexedIndirect, OperationType.Read, adc)
        case 0x71 => createOperation(AddressingMode.IndirectIndexed, OperationType.Read, adc)

        // AND (Logical AND)
        case 0x29 => createOperation(AddressingMode.Immediate, OperationType.Read, and)
        case 0x25 => createOperation(AddressingMode.ZeroPage, OperationType.Read, and)
        case 0x35 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Read, and, xIndex)
        case 0x2D => createOperation(AddressingMode.Absolute, OperationType.Read, and)
        case 0x3D => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, and, xIndex)
        case 0x39 => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, and, yIndex)
        case 0x21 => createOperation(AddressingMode.IndexedIndirect, OperationType.Read, and)
        case 0x31 => createOperation(AddressingMode.IndirectIndexed, OperationType.Read, and)

        // ASL (Arithmetic Shift Left)
        case 0x0A => createOperation(AddressingMode.Accumulator, OperationType.ReadModifyWrite, asl)
        case 0x06 => createOperation(AddressingMode.ZeroPage, OperationType.ReadModifyWrite, asl)
        case 0x16 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.ReadModifyWrite, asl, xIndex)
        case 0x0E => createOperation(AddressingMode.Absolute, OperationType.ReadModifyWrite, asl)
        case 0x1E => createOperation(AddressingMode.AbsoluteIndexed, OperationType.ReadModifyWrite, asl, xIndex)
        
        // Branching Instructions
        case 0x90 => createRelativeOperation(bcc)
        case 0xB0 => createRelativeOperation(bcs)
        case 0xF0 => createRelativeOperation(beq)
        case 0x30 => createRelativeOperation(bmi)
        case 0xD0 => createRelativeOperation(bne)
        case 0x10 => createRelativeOperation(bpl)
        case 0x50 => createRelativeOperation(bvc)
        case 0x70 => createRelativeOperation(bvs)

        // BIT (Bit test)
        case 0x24 => createOperation(AddressingMode.ZeroPage, OperationType.Read, bit)
        case 0x2C => createOperation(AddressingMode.Absolute, OperationType.Read, bit)

        // BRK (Force interrupt)
        case 0x00 => createImpliedOperation(brk)

        // Status flag clearing instructions
        case 0x18 => createImpliedOperation(clc)
        case 0xD8 => createImpliedOperation(cld)
        case 0x58 => createImpliedOperation(cli)
        case 0xB8 => createImpliedOperation(clv)

        // CMP (Compare accumulator)
        case 0xC9 => createOperation(AddressingMode.Immediate, OperationType.Read, cmp)
        case 0xC5 => createOperation(AddressingMode.ZeroPage, OperationType.Read, cmp)
        case 0xD5 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Read, cmp, xIndex)
        case 0xCD => createOperation(AddressingMode.Absolute, OperationType.Read, cmp)
        case 0xDD => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, cmp, xIndex)
        case 0xD9 => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, cmp, yIndex)
        case 0xC1 => createOperation(AddressingMode.IndexedIndirect, OperationType.Read, cmp)
        case 0xD1 => createOperation(AddressingMode.IndirectIndexed, OperationType.Read, cmp)

        // CPX (Compare X Register)
        case 0xE0 => createOperation(AddressingMode.Immediate, OperationType.Read, cpx)
        case 0xE4 => createOperation(AddressingMode.ZeroPage, OperationType.Read, cpx)
        case 0xEC => createOperation(AddressingMode.Absolute, OperationType.Read, cpx)

        // CPY (Compare Y Register)
        case 0xC0 => createOperation(AddressingMode.Immediate, OperationType.Read, cpy)
        case 0xC4 => createOperation(AddressingMode.ZeroPage, OperationType.Read, cpy)
        case 0xCC => createOperation(AddressingMode.Absolute, OperationType.Read, cpy)

        // DEC (Decrement Memory)
        case 0xC6 => createOperation(AddressingMode.ZeroPage, OperationType.ReadModifyWrite, dec)
        case 0xD6 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.ReadModifyWrite, dec, xIndex)
        case 0xCE => createOperation(AddressingMode.Absolute, OperationType.ReadModifyWrite, dec)
        case 0xDE => createOperation(AddressingMode.AbsoluteIndexed, OperationType.ReadModifyWrite, dec, xIndex)

        // DEX (Decrement X register)
        case 0xCA => createImpliedOperation(dex)

        // DEY (Decrement Y register)
        case 0x88 => createImpliedOperation(dey)

        // JMP (Jump)
        case 0x4C => createOperation(AddressingMode.Absolute, OperationType.Other, jmp)
        case 0x6C => createOperation(AddressingMode.Indirect, OperationType.Other, jmp)

        // LDA (Load accumulator)
        case 0xA9 => createOperation(AddressingMode.Immediate, OperationType.Read, lda)
        case 0xA5 => createOperation(AddressingMode.ZeroPage, OperationType.Read, lda)
        case 0xB5 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Read, lda, xIndex)
        case 0xAD => createOperation(AddressingMode.Absolute, OperationType.Read, lda)
        case 0xBD => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, lda, xIndex)
        case 0xB9 => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, lda, yIndex)
        case 0xA1 => createOperation(AddressingMode.IndexedIndirect, OperationType.Read, lda)
        case 0xB1 => createOperation(AddressingMode.IndirectIndexed, OperationType.Read, lda)

        // LDA (Load X register)
        case 0xA2 => createOperation(AddressingMode.Immediate, OperationType.Read, ldx)
        case 0xA6 => createOperation(AddressingMode.ZeroPage, OperationType.Read, ldx)
        case 0xB6 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Read, ldx, yIndex)
        case 0xAE => createOperation(AddressingMode.Absolute, OperationType.Read, ldx)
        case 0xBE => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, ldx, yIndex)

        // LDA (Load Y register)
        case 0xA0 => createOperation(AddressingMode.Immediate, OperationType.Read, ldy)
        case 0xA4 => createOperation(AddressingMode.ZeroPage, OperationType.Read, ldy)
        case 0xB4 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Read, ldy, xIndex)
        case 0xAC => createOperation(AddressingMode.Absolute, OperationType.Read, ldy)
        case 0xBC => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, ldy, xIndex)

        // NOP (No operation)
        case 0xEA => createImpliedOperation(nop)

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

    private def pushToStack(data: Int): Unit = {
        stackPointer -= 1
        memory(stackPointer) = data
    }

    private def popFromStack(): Int = {
        val data = memory(stackPointer)
        stackPointer += 1
        data
    }


    /* ------ Flag updating methods ------ */

    // TODO: look into beter flag masking options

    private def updateZeroFlag(condition: Boolean): Unit = {
        if (condition) status |= ZeroFlagMask
        else status &= ~ZeroFlagMask & 0xFF
    }

    private def updateNegativeFlag(register: Int): Unit = {
        val NewNegativeFlag = register & NegativeFlagMask
        status |= NewNegativeFlag
    }

    private def updateCarryFlag(condition: Boolean): Unit = {
        if (condition) status |= CarryFlagMask
        else status &= ~CarryFlagMask & 0xFF
    }


    private def createOperation(
        addressingMode: AddressingMode.T,
        operationType: OperationType.T,
        operation: Option[Int] => Unit,
        index: Int = 0
    ): Unit = addressingMode match {

        case AddressingMode.Accumulator => Cycles.add(() => operation(None))

        case AddressingMode.Immediate => {
            Cycles.add(() => {
                operation(Some(memory(programCounter)))
                programCounter += 1
            })
        }

        case AddressingMode.ZeroPage => {
            Cycles.add(fetchNextByte)
            if (operationType == OperationType.ReadModifyWrite) {
                Cycles.add(() => Cycles.storeByte(memory(Cycles.getNextStoredByte())))
                Cycles.add() // Delay one cycle to get correct 5 cycle operation.
                Cycles.add(() => {
                    val Data = Cycles.getNextStoredByte()
                    operation(Some(Data))
                })
            }
            else Cycles.add(() => {
                val Data = memory(Cycles.getNextStoredByte())
                operation(Some(Data))
            })
        }

        case AddressingMode.ZeroPageIndexed => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val Address = (Cycles.getNextStoredByte() + index) & 0xFF
                Cycles.storeByte(Address)
            })
            Cycles.add(() => {
                val Data = memory(Cycles.getNextStoredByte())
                operation(Some(Data))
            })
        }

        case AddressingMode.Absolute => {
            if (operationType == OperationType.Other) { // JMP
                Cycles.add(fetchNextByte)
                Cycles.add(() => {
                    val AddressLowByte = Cycles.getNextStoredByte()
                    val AddressHighByte = memory(programCounter) << 8
                    operation(Some(AddressHighByte | AddressLowByte))
                })
            } else {
                Cycles.add(fetchNextByte)
                Cycles.add(fetchNextByte)
                Cycles.add(() => operation(Some(getStoredAddress())))
            }
        }

        case AddressingMode.AbsoluteIndexed => {
            Cycles.add(fetchNextByte)
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = Cycles.getNextStoredByte() << 8
                val Address = (AddressHighByte | AddressLowByte) + index
                val Data = Some(memory(Address))

                // Extra cycle needed if page boundary crossed
                if (AddressLowByte + index < 0x100) operation(Data)
                else Cycles.add(() => operation(Data))
            })
        }

        // JMP Only
        case AddressingMode.Indirect => {
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

        case AddressingMode.IndexedIndirect => {
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
                val Data = memory(getStoredAddress())
                operation(Some(Data))
            })
        }

        case AddressingMode.IndirectIndexed => {
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
                val Data = Some(memory(Address))

                // Extra cycle needed if page boundary crossed
                if (AddressLowByte + yIndex < 0x100) operation(Data)
                else Cycles.add(() => operation(Data))
            })
        }

        case _ =>

    }

    private def createImpliedOperation(operation: () => Unit): Unit = operation()

    private def createRelativeOperation(branchCondition: () => Boolean): Unit = {
        Cycles.add(fetchNextByte)
        if (branchCondition()) {
            Cycles.add(() => {
                val PreviousPCPage = programCounter & 0xFF00

                // Calculate signed offset (Byte -> Int)
                val Offset = (~0 << 8) | Cycles.getNextStoredByte()
                programCounter += Offset

                // Add one cycle after branch, two if to a different page
                Cycles.add()
                if ((programCounter & 0xFF00) != PreviousPCPage) Cycles.add()
            })
        }
        else programCounter += 1
    }


    /* ------- Opcodes ------- */

    private def adc(data: Option[Int]): Unit = {
        val PreviousSignBit = accumulator & NegativeFlagMask

        accumulator += data.get
        accumulator += status & CarryFlagMask

        // Update Carry flag
        val NewCarryFlag = (accumulator & 0x100) >> 8
        accumulator &= 0xFF
        status |= NewCarryFlag

        // Update Overflow flag
        if ((accumulator & NegativeFlagMask) != PreviousSignBit) status |= OverflowFlagMask
        else status &= ~OverflowFlagMask & 0xFF

        updateZeroFlag(accumulator == 0)
        updateNegativeFlag(accumulator)
    }

    private def and(data: Option[Int]): Unit = {
        accumulator &= data.get
        updateZeroFlag(accumulator == 0)
        updateNegativeFlag(accumulator)
    }

    private def asl(option: Option[Int]): Unit = option match {
        case Some(data) => {
            // Set Carry flag to previous bit 7
            status |= ((memory(data) & NegativeFlagMask) >> 7)
            memory(data) <<= 1
            updateZeroFlag(memory(data) == 0)
            updateNegativeFlag(memory(data))
        }
        case None => {
            status |= ((accumulator & NegativeFlagMask) >> 7)
            accumulator <<= 1
            updateZeroFlag(accumulator == 0)
            updateNegativeFlag(accumulator)
        }
    }

    private def bcc(): Boolean = (status & CarryFlagMask) == 0

    private def bcs(): Boolean = !bcc()

    private def beq(): Boolean = (status & ZeroFlagMask) >> 1 == 1

    private def bit(option: Option[Int]): Unit = {
        val data = option.get
        if ((accumulator & data) > 0) status |= ZeroFlagMask
        else status &= ~ZeroFlagMask
        status |= data & OverflowFlagMask
        status |= data & NegativeFlagMask
    }

    private def bne(): Boolean = !beq()

    private def bmi(): Boolean = (status & NegativeFlagMask) >> 7 == 1

    private def bpl(): Boolean = !bmi()

    private def brk(): Unit = {
        Cycles.add(() => programCounter += 1)
        Cycles.add(() => pushToStack((programCounter & 0xFF00) >> 8))
        Cycles.add(() => pushToStack(programCounter & 0xFF))
        Cycles.add(() => {
            status |= BFlagMask
            pushToStack(status)
        })
        Cycles.add(() => Cycles.storeByte(memory(0xFFFE)))
        Cycles.add(() => {
            val ProgramCounterLowByte = Cycles.getNextStoredByte()
            val ProgramCounterHighByte = memory(0xFFFF) << 8
            programCounter = ProgramCounterHighByte | ProgramCounterLowByte
        })
    }

    private def bvc(): Boolean = (status & OverflowFlagMask) >> 6 == 0

    private def bvs(): Boolean = !bvc()

    private def clc(): Unit = Cycles.add(() => status &= ~CarryFlagMask & 0xFF)

    private def cld(): Unit = Cycles.add(() => status &= ~DecimalFlagMask & 0xFF) 

    private def cli(): Unit = Cycles.add(() => status &= ~InterruptDisableFlagMask & 0xFF) 

    private def clv(): Unit = Cycles.add(() => status &= ~OverflowFlagMask & 0xFF) 

    private def cmp(option: Option[Int]): Unit = {
        val data = option.get
        updateCarryFlag(accumulator >= data)
        updateZeroFlag(accumulator == data)
        updateNegativeFlag(accumulator)
    }

    private def cpx(option: Option[Int]): Unit = {
        val data = option.get
        updateCarryFlag(xIndex >= data)
        updateZeroFlag(xIndex == data)
        updateNegativeFlag(xIndex)
    }

    private def cpy(option: Option[Int]): Unit = {
        val data = option.get
        updateCarryFlag(yIndex >= data)
        updateZeroFlag(yIndex == data)
        updateNegativeFlag(yIndex)
    }

    private def dec(option: Option[Int]): Unit = {
        val data = option.get
        memory(data) -= 1
        updateZeroFlag(memory(data) == 0)
        updateNegativeFlag(memory(data))
    }

    private def dex(): Unit = Cycles.add(() => {
        xIndex -= 1
        updateZeroFlag(xIndex == 0)
        updateNegativeFlag(xIndex)
    })

    private def dey(): Unit = Cycles.add(() => {
        yIndex -= 1
        updateZeroFlag(yIndex == 0)
        updateNegativeFlag(yIndex)
    })

    private def jmp(data: Option[Int]): Unit = programCounter = data.get

    private def lda(data: Option[Int]): Unit = {
        accumulator = data.get
        updateZeroFlag(accumulator == 0)
        updateNegativeFlag(accumulator)
    }

    private def ldx(data: Option[Int]): Unit = {
        xIndex = data.get
        updateZeroFlag(xIndex == 0)
        updateNegativeFlag(xIndex)
    }

    private def ldy(data: Option[Int]): Unit = {
        yIndex = data.get
        updateZeroFlag(yIndex == 0)
        updateNegativeFlag(yIndex)
    }

    private def nop(): Unit = Cycles.add()


}
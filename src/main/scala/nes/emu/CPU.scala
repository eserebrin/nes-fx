package nes.emu

import scala.collection.mutable

class CPU(memory: Mapper) {

    // CPU registers
    private var accumulator = 0
    private var xIndex = 0
    private var yIndex = 0
    private var programCounter = 0xC000
    private var stackPointer = 0xFD
    private var status = 0x24

    // Status Flags
    private val CarryFlagMask = 1
    private val ZeroFlagMask = 1 << 1
    private val InterruptDisableFlagMask = 1 << 2
    private val DecimalFlagMask = 1 << 3
    private val BFlagMask = 0x30 // 00110000
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
        def clearStoredData(): Unit = storedData = mutable.Buffer[Int]()

        def showStoredData(): String = storedData.mkString(", ")

    }

    // Total number of CPU cycles run since power on
    // NOTE: this should be changed to private once CPU works
    var cycleCount = 0L

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
        // Console.addLog(s"Stored data: ${Cycles.showStoredData()}")
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

    def createSingleLineTextLogOutput(): String = {
        if (Cycles.isOperationFinished())
            f"$programCounter%04X   A:$accumulator%02X X:$xIndex%02X Y:$yIndex%02X P:$status%02X SP:$stackPointer%02X   CYC:${cycleCount + 7}\n"
        else ""
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

        // EOR (Exclusive OR)
        case 0x49 => createOperation(AddressingMode.Immediate, OperationType.Read, eor)
        case 0x45 => createOperation(AddressingMode.ZeroPage, OperationType.Read, eor)
        case 0x55 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Read, eor, xIndex)
        case 0x4D => createOperation(AddressingMode.Absolute, OperationType.Read, eor)
        case 0x5D => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, eor, xIndex)
        case 0x59 => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, eor, yIndex)
        case 0x41 => createOperation(AddressingMode.IndexedIndirect, OperationType.Read, eor)
        case 0x51 => createOperation(AddressingMode.IndirectIndexed, OperationType.Read, eor)

        // INC (Increment Memory)
        case 0xE6 => createOperation(AddressingMode.ZeroPage, OperationType.ReadModifyWrite, inc)
        case 0xF6 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.ReadModifyWrite, inc, xIndex)
        case 0xEE => createOperation(AddressingMode.Absolute, OperationType.ReadModifyWrite, inc)
        case 0xFE => createOperation(AddressingMode.AbsoluteIndexed, OperationType.ReadModifyWrite, inc, xIndex)

        // INX (Increment X register)
        case 0xE8 => createImpliedOperation(inx)

        // INY (Increment Y register)
        case 0xC8 => createImpliedOperation(iny)

        // JMP (Jump)
        case 0x4C => createOperation(AddressingMode.Absolute, OperationType.Other, jmp)
        case 0x6C => createOperation(AddressingMode.Indirect, OperationType.Other, jmp)

        // JSR (Jump to Subroutine) -- Technically absolute, not implied
        case 0x20 => createImpliedOperation(jsr)

        // LDA (Load accumulator)
        case 0xA9 => createOperation(AddressingMode.Immediate, OperationType.Read, lda)
        case 0xA5 => createOperation(AddressingMode.ZeroPage, OperationType.Read, lda)
        case 0xB5 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Read, lda, xIndex)
        case 0xAD => createOperation(AddressingMode.Absolute, OperationType.Read, lda)
        case 0xBD => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, lda, xIndex)
        case 0xB9 => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, lda, yIndex)
        case 0xA1 => createOperation(AddressingMode.IndexedIndirect, OperationType.Read, lda)
        case 0xB1 => createOperation(AddressingMode.IndirectIndexed, OperationType.Read, lda)

        // LDX (Load X register)
        case 0xA2 => createOperation(AddressingMode.Immediate, OperationType.Read, ldx)
        case 0xA6 => createOperation(AddressingMode.ZeroPage, OperationType.Read, ldx)
        case 0xB6 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Read, ldx, yIndex)
        case 0xAE => createOperation(AddressingMode.Absolute, OperationType.Read, ldx)
        case 0xBE => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, ldx, yIndex)

        // LDY (Load Y register)
        case 0xA0 => createOperation(AddressingMode.Immediate, OperationType.Read, ldy)
        case 0xA4 => createOperation(AddressingMode.ZeroPage, OperationType.Read, ldy)
        case 0xB4 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Read, ldy, xIndex)
        case 0xAC => createOperation(AddressingMode.Absolute, OperationType.Read, ldy)
        case 0xBC => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, ldy, xIndex)

        // LSR (Logical Shift Right)
        case 0x4A => createOperation(AddressingMode.Accumulator, OperationType.ReadModifyWrite, lsr)
        case 0x46 => createOperation(AddressingMode.ZeroPage, OperationType.ReadModifyWrite, lsr)
        case 0x56 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.ReadModifyWrite, lsr, xIndex)
        case 0x4E => createOperation(AddressingMode.Absolute, OperationType.ReadModifyWrite, lsr)
        case 0x5E => createOperation(AddressingMode.AbsoluteIndexed, OperationType.ReadModifyWrite, lsr, xIndex)

        // ORA (Logical Inclusive OR)
        case 0x09 => createOperation(AddressingMode.Immediate, OperationType.Read, ora)
        case 0x05 => createOperation(AddressingMode.ZeroPage, OperationType.Read, ora)
        case 0x15 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Read, ora, xIndex)
        case 0x0D => createOperation(AddressingMode.Absolute, OperationType.Read, ora)
        case 0x1D => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, ora, xIndex)
        case 0x19 => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, ora, yIndex)
        case 0x01 => createOperation(AddressingMode.IndexedIndirect, OperationType.Read, ora)
        case 0x11 => createOperation(AddressingMode.IndirectIndexed, OperationType.Read, ora)

        // NOP (No operation)
        case 0xEA => createImpliedOperation(nop)

        // Stack accessing instructions
        case 0x48 => createImpliedOperation(pha)
        case 0x08 => createImpliedOperation(php)
        case 0x68 => createImpliedOperation(pla)
        case 0x28 => createImpliedOperation(plp)

        // ROL (Rotate left)
        case 0x2A => createOperation(AddressingMode.Accumulator, OperationType.ReadModifyWrite, rol)
        case 0x26 => createOperation(AddressingMode.ZeroPage, OperationType.ReadModifyWrite, rol)
        case 0x36 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.ReadModifyWrite, rol, xIndex)
        case 0x2E => createOperation(AddressingMode.Absolute, OperationType.ReadModifyWrite, rol)
        case 0x3E => createOperation(AddressingMode.AbsoluteIndexed, OperationType.ReadModifyWrite, rol, xIndex)

        // ROR (Rotate right)
        case 0x6A => createOperation(AddressingMode.Accumulator, OperationType.ReadModifyWrite, ror)
        case 0x66 => createOperation(AddressingMode.ZeroPage, OperationType.ReadModifyWrite, ror)
        case 0x76 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.ReadModifyWrite, ror, xIndex)
        case 0x6E => createOperation(AddressingMode.Absolute, OperationType.ReadModifyWrite, ror)
        case 0x7E => createOperation(AddressingMode.AbsoluteIndexed, OperationType.ReadModifyWrite, ror, xIndex)

        // RTI (Return from Interrupt)
        case 0x40 => createImpliedOperation(rti)

        // RTS (Return from subroutine)
        case 0x60 => createImpliedOperation(rts)

        // SBC (Subtract with carry)
        case 0xE9 => createOperation(AddressingMode.Immediate, OperationType.Read, sbc)
        case 0xE5 => createOperation(AddressingMode.ZeroPage, OperationType.Read, sbc)
        case 0xF5 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Read, sbc, xIndex)
        case 0xED => createOperation(AddressingMode.Absolute, OperationType.Read, sbc)
        case 0xFD => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, sbc, xIndex)
        case 0xF9 => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Read, sbc, yIndex)
        case 0xE1 => createOperation(AddressingMode.IndexedIndirect, OperationType.Read, sbc)
        case 0xF1 => createOperation(AddressingMode.IndirectIndexed, OperationType.Read, sbc)

        // Flag setting instructions
        case 0x38 => createImpliedOperation(sec)
        case 0xF8 => createImpliedOperation(sed)
        case 0x78 => createImpliedOperation(sei)

        // STA (Store accumulator)
        case 0x85 => createOperation(AddressingMode.ZeroPage, OperationType.Write, sta)
        case 0x95 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Write, sta, xIndex)
        case 0x8D => createOperation(AddressingMode.Absolute, OperationType.Write, sta)
        case 0x9D => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Write, sta, xIndex)
        case 0x99 => createOperation(AddressingMode.AbsoluteIndexed, OperationType.Write, sta, yIndex)
        case 0x81 => createOperation(AddressingMode.IndexedIndirect, OperationType.Write, sta)
        case 0x91 => createOperation(AddressingMode.IndirectIndexed, OperationType.Write, sta)

        // STX (Store X register)
        case 0x86 => createOperation(AddressingMode.ZeroPage, OperationType.Write, stx)
        case 0x96 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Write, stx, yIndex)
        case 0x8E => createOperation(AddressingMode.Absolute, OperationType.Write, stx)

        // STY (Store Y register)
        case 0x84 => createOperation(AddressingMode.ZeroPage, OperationType.Write, sty)
        case 0x94 => createOperation(AddressingMode.ZeroPageIndexed, OperationType.Write, sty, xIndex)
        case 0x8C => createOperation(AddressingMode.Absolute, OperationType.Write, sty)

        // Transfer instructions
        case 0xAA => createImpliedOperation(tax)
        case 0xA8 => createImpliedOperation(tay)
        case 0xBA => createImpliedOperation(tsx)
        case 0x8A => createImpliedOperation(txa)
        case 0x9A => createImpliedOperation(txs)
        case 0x98 => createImpliedOperation(tya)

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
        memory(0x100 |  stackPointer) = data
        stackPointer -= 1
        stackPointer &= 0xFF // wrap around at stack overflow
    }

    private def pullFromStack(): Int = {
        stackPointer += 1
        stackPointer &= 0xFF // wrap around at stack underflow
        memory(0x100 | stackPointer)
    }

    private def addCycles(num: Int): Unit = for (i <- 1 to num) Cycles.add()

    private def createSignedIntFromByte(num: Int): Int = {
        if ((num & NegativeFlagMask) >> 7 == 1) num | (~0 << 8)
        else num
    }

    /* ------ Flag updating methods ------ */

    private def updateZeroFlag(condition: Boolean): Unit = {
        if (condition) status |= ZeroFlagMask
        else status &= ~ZeroFlagMask & 0xFF
    }

    private def updateNegativeFlag(register: Int): Unit = {
        val NewNegativeFlag = register & NegativeFlagMask
        if (NewNegativeFlag > 0) status |= NewNegativeFlag
        else status &= ~NegativeFlagMask & 0xFF
    }

    private def updateCarryFlag(condition: Boolean): Unit = {
        if (condition) status |= CarryFlagMask
        else status &= ~CarryFlagMask & 0xFF
    }

    private def updateOverflowFlag(condition: Boolean): Unit = {
        if (condition) status |= OverflowFlagMask
        else status &= ~OverflowFlagMask & 0xFF
    }

    // start the operation by passing in the correct address to the operation
    private def createOperation(
        addressingMode: AddressingMode.T,
        operationType: OperationType.T,
        operation: Option[Int] => Unit,
        index: Int = 0
    ): Unit = addressingMode match {

        case AddressingMode.Accumulator => Cycles.add(() => operation(None))

        case AddressingMode.Immediate => {
            Cycles.add(() => {
                operation(Some(programCounter))
                programCounter += 1
            })
        }

        case AddressingMode.ZeroPage => {
            Cycles.add(fetchNextByte)
            if (operationType == OperationType.ReadModifyWrite) addCycles(2)
            Cycles.add(() => operation(Some(Cycles.getNextStoredByte())))
        }

        case AddressingMode.ZeroPageIndexed => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val Address = (Cycles.getNextStoredByte() + index) & 0xFF
                Cycles.storeByte(Address)
            })
            if (operationType == OperationType.ReadModifyWrite) addCycles(2)
            Cycles.add(() => operation(Some(Cycles.getNextStoredByte())))
        }

        case AddressingMode.Absolute => {
            Cycles.add(fetchNextByte)
            if (operationType == OperationType.Other) { // JMP
                Cycles.add(() => {
                    val AddressLowByte = Cycles.getNextStoredByte()
                    val AddressHighByte = memory(programCounter) << 8
                    operation(Some(AddressHighByte | AddressLowByte))
                })
            } else {
                Cycles.add(fetchNextByte)
                if (operationType == OperationType.ReadModifyWrite) addCycles(2)
                Cycles.add(() => operation(Some(getStoredAddress())))
            }
        }

        case AddressingMode.AbsoluteIndexed => {
            Cycles.add(fetchNextByte)
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = Cycles.getNextStoredByte() << 8
                val Address = ((AddressHighByte | AddressLowByte) + index) & 0xFFFF

                if (operationType == OperationType.Read) {
                    // Extra cycle needed if page boundary crossed
                    if (AddressLowByte + index < 0x100) operation(Some(Address))
                    else Cycles.add(() => operation(Some(Address)))
                }
                else {
                    if (operationType == OperationType.ReadModifyWrite) addCycles(2)
                    Cycles.add(() => operation(Some(Address)))
                }
            })
        }

        // JMP Only
        case AddressingMode.Indirect => {
            Cycles.add(fetchNextByte)
            Cycles.add(fetchNextByte)
            Cycles.add()
            Cycles.add(() => {
                val PointerLowByte = Cycles.getNextStoredByte()
                val PointerHighByte = Cycles.getNextStoredByte()

                val AddressLowByte = memory((PointerHighByte << 8) | PointerLowByte)
                val AddressHighByte = if (PointerLowByte == 0xFF) memory(PointerHighByte << 8)
                                      else memory((PointerHighByte << 8) | (PointerLowByte + 1))

                operation(Some((AddressHighByte << 8) | AddressLowByte))
            })
        }

        case AddressingMode.IndexedIndirect => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => Cycles.storeByte((Cycles.getNextStoredByte() + xIndex) & 0xFF))
            Cycles.add(() => Cycles.storeByte(memory(Cycles.peekNextStoredByte())))
            Cycles.add(() => Cycles.storeByte(memory((Cycles.getNextStoredByte() + 1) & 0xFF)))
            if (operationType == OperationType.ReadModifyWrite) addCycles(2)
            Cycles.add(() => operation(Some(getStoredAddress())))
        }

        case AddressingMode.IndirectIndexed => {
            Cycles.add(fetchNextByte)
            Cycles.add(() => Cycles.storeByte(memory(Cycles.peekNextStoredByte())))
            Cycles.add(() => Cycles.storeByte(memory((Cycles.getNextStoredByte() + 1) & 0xFF)))
            Cycles.add(() => {
                val AddressLowByte = Cycles.getNextStoredByte()
                val AddressHighByte = Cycles.getNextStoredByte() << 8
                val Address = ((AddressHighByte | AddressLowByte) + yIndex) & 0xFFFF

                if (operationType == OperationType.Read) {
                    // Extra cycle needed if page boundary crossed
                    if (AddressLowByte + yIndex < 0x100) operation(Some(Address))
                    else Cycles.add(() => operation(Some(Address)))
                }
                else {
                    if (operationType == OperationType.ReadModifyWrite) addCycles(2)
                    Cycles.add(() => operation(Some(Address)))
                }
            })
        }

        case _ =>

    }

    private def createImpliedOperation(operation: () => Unit): Unit = operation()

    private def createRelativeOperation(branchCondition: () => Boolean): Unit = {
        if (branchCondition()) {
            Cycles.add(fetchNextByte)
            Cycles.add(() => {
                val PreviousPCPage = programCounter & 0xFF00

                programCounter += createSignedIntFromByte(Cycles.getNextStoredByte())

                // Add extra cycle if branching to a different page
                if ((programCounter & 0xFF00) != PreviousPCPage) Cycles.add()
            })
        }
        else Cycles.add(() => programCounter += 1)
    }


    /* ------- Opcodes ------- */

    private def adc(address: Option[Int]): Unit = {
        val AccumulatorPreviousSignBit = accumulator & NegativeFlagMask
        val MemoryPreviousSignBit = memory(address.get) & NegativeFlagMask

        accumulator += memory(address.get)
        accumulator += status & CarryFlagMask

        // Update Carry flag
        val NewCarryFlag = accumulator & 0x100
        accumulator &= 0xFF
        updateCarryFlag(NewCarryFlag > 0)

        val SetOverflow = MemoryPreviousSignBit == AccumulatorPreviousSignBit && AccumulatorPreviousSignBit != (accumulator & NegativeFlagMask)
        updateOverflowFlag(SetOverflow)
        updateZeroFlag(accumulator == 0)
        updateNegativeFlag(accumulator)
    }

    private def and(address: Option[Int]): Unit = {
        accumulator &= memory(address.get)
        updateZeroFlag(accumulator == 0)
        updateNegativeFlag(accumulator)
    }

    private def asl(option: Option[Int]): Unit = option match {
        case Some(address) => {
            // Set Carry flag to previous bit 7
            updateCarryFlag((memory(address) & NegativeFlagMask) > 0)
            memory(address) <<= 1
            memory(address) &= 0xFF
            updateZeroFlag(memory(address) == 0)
            updateNegativeFlag(memory(address))
        }
        case None => {
            updateCarryFlag((accumulator & NegativeFlagMask) > 0)
            accumulator <<= 1
            accumulator &= 0xFF
            updateZeroFlag(accumulator == 0)
            updateNegativeFlag(accumulator)
        }
    }

    private def bcc(): Boolean = (status & CarryFlagMask) == 0

    private def bcs(): Boolean = !bcc()

    private def beq(): Boolean = (status & ZeroFlagMask) >> 1 == 1

    private def bit(option: Option[Int]): Unit = {
        val data = memory(option.get)
        if ((accumulator & data) == 0) status |= ZeroFlagMask
        else status &= ~ZeroFlagMask
        updateOverflowFlag((data & OverflowFlagMask) > 0)
        updateNegativeFlag(data)
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

    private def cmp(address: Option[Int]): Unit = {
        val data = memory(address.get)
        updateCarryFlag(accumulator >= data)
        updateZeroFlag(accumulator == data)
        updateNegativeFlag(accumulator - data)
    }

    private def cpx(address: Option[Int]): Unit = {
        val data = memory(address.get)
        updateCarryFlag(xIndex >= data)
        updateZeroFlag(xIndex == data)
        updateNegativeFlag(xIndex - data)
    }

    private def cpy(address: Option[Int]): Unit = {
        val data = memory(address.get)
        updateCarryFlag(yIndex >= data)
        updateZeroFlag(yIndex == data)
        updateNegativeFlag(yIndex - data)
    }

    private def dec(option: Option[Int]): Unit = {
        val address = option.get
        memory(address) -= 1
        memory(address) &= 0xFF
        updateZeroFlag(memory(address) == 0)
        updateNegativeFlag(memory(address))
    }

    private def dex(): Unit = Cycles.add(() => {
        xIndex -= 1
        xIndex &= 0xFF
        updateZeroFlag(xIndex == 0)
        updateNegativeFlag(xIndex)
    })

    private def dey(): Unit = Cycles.add(() => {
        yIndex -= 1
        yIndex &= 0xFF
        updateZeroFlag(yIndex == 0)
        updateNegativeFlag(yIndex)
    })

    private def eor(address: Option[Int]): Unit = {
        accumulator ^= memory(address.get)
        updateZeroFlag(accumulator == 0)
        updateNegativeFlag(accumulator)
    }

    private def inc(option: Option[Int]): Unit = {
        val address = option.get
        memory(address) += 1
        memory(address) &= 0xFF
        updateZeroFlag(memory(address) == 0)
        updateNegativeFlag(memory(address))
    }

    private def inx(): Unit = Cycles.add(() => {
        xIndex += 1
        xIndex &= 0xFF
        updateZeroFlag(xIndex == 0)
        updateNegativeFlag(xIndex)
    })

    private def iny(): Unit = Cycles.add(() => {
        yIndex += 1
        yIndex &= 0xFF
        updateZeroFlag(yIndex == 0)
        updateNegativeFlag(yIndex)
    })

    private def jmp(address: Option[Int]): Unit = programCounter = address.get

    private def jsr(): Unit = {
        Cycles.add(fetchNextByte)
        Cycles.add()
        Cycles.add(() => pushToStack((programCounter & 0xFF00) >> 8))
        Cycles.add(() => pushToStack(programCounter & 0xFF))
        Cycles.add(() => {
            val AddressLowByte = Cycles.getNextStoredByte()
            val AddressHighByte = memory(programCounter) << 8
            programCounter = AddressHighByte | AddressLowByte
        })
    }

    private def lda(address: Option[Int]): Unit = {
        accumulator = memory(address.get)
        updateZeroFlag(accumulator == 0)
        updateNegativeFlag(accumulator)
    }

    private def ldx(address: Option[Int]): Unit = {
        xIndex = memory(address.get)
        updateZeroFlag(xIndex == 0)
        updateNegativeFlag(xIndex)
    }

    private def ldy(address: Option[Int]): Unit = {
        yIndex = memory(address.get)
        updateZeroFlag(yIndex == 0)
        updateNegativeFlag(yIndex)
    }

    private def lsr(option: Option[Int]): Unit = option match {
        case Some(address) => {
            updateCarryFlag((memory(address) & CarryFlagMask) > 0)
            memory(address) >>= 1
            memory(address) &= 0xFF
            updateZeroFlag(memory(address) == 0)
            updateNegativeFlag(memory(address))
        }
        case None => {
            updateCarryFlag((accumulator & CarryFlagMask) > 0)
            accumulator >>= 1
            accumulator &= 0xFF
            updateZeroFlag(accumulator == 0)
            updateNegativeFlag(accumulator)
        }
    }

    private def nop(): Unit = Cycles.add()

    private def ora(address: Option[Int]): Unit = {
        accumulator |= memory(address.get)
        updateZeroFlag(accumulator == 0)
        updateNegativeFlag(accumulator)
    }

    private def pha(): Unit = {
        Cycles.add()
        Cycles.add(() => pushToStack(accumulator))
    }

    private def php(): Unit = {
        Cycles.add()
        Cycles.add(() => pushToStack(status | BFlagMask)) // Setting the B flag is a CPU quirk
    }

    private def pla(): Unit = {
        addCycles(2)
        Cycles.add(() => {
            accumulator = pullFromStack()
            updateZeroFlag(accumulator == 0)
            updateNegativeFlag(accumulator)
        })
    }

    private def plp(): Unit = {
        addCycles(2)
        Cycles.add(() => status = (pullFromStack() & 0xCF) | (status & 0x30)) // Ignore B flag bits
    }

    private def rol(option: Option[Int]): Unit = option match {
        case Some(address) => {
            // New carry flag is the old bit 7
            val NewCarryFlag = (memory(address) & (1 << 7)) >> 7

            memory(address) <<= 1
            memory(address) |= status & CarryFlagMask
            memory(address) &= 0xFF

            status |= NewCarryFlag
            updateZeroFlag(memory(address) == 0)
            updateNegativeFlag(memory(address))
        }
        case None => {
            // New carry flag is the old bit 7
            val NewCarryFlag = (accumulator & (1 << 7)) >> 7

            accumulator <<= 1
            accumulator |= status & CarryFlagMask
            accumulator &= 0xFF

            status |= NewCarryFlag
            updateZeroFlag(accumulator == 0)
            updateNegativeFlag(accumulator)
        }
    }

    private def ror(option: Option[Int]): Unit = option match {
        case Some(address) => {
            val NewCarryFlag = memory(address) & 1

            memory(address) >>= 1
            memory(address) |= (status & CarryFlagMask) << 7

            status |= NewCarryFlag
            updateZeroFlag(memory(address) == 0)
            updateNegativeFlag(memory(address))
        }
        case None => {
            val NewCarryFlag = accumulator & 1

            accumulator >>= 1
            accumulator |= (status & CarryFlagMask) << 7

            status |= NewCarryFlag
            updateZeroFlag(accumulator == 0)
            updateNegativeFlag(accumulator)
        }
    }

    private def rti(): Unit = {
        addCycles(2)
        Cycles.add(() => status = (pullFromStack() & 0xCF) | (status & 0x30)) // Ignore B flag bits
        Cycles.add(() => Cycles.storeByte(pullFromStack()))
        Cycles.add(() => {
            val ProgramCounterLowByte = Cycles.getNextStoredByte()
            val ProgramCounterHighByte = pullFromStack()
            programCounter = (ProgramCounterHighByte << 8) | ProgramCounterLowByte
        })
    }

    private def rts(): Unit = {
        addCycles(2)
        Cycles.add(() => Cycles.storeByte(pullFromStack()))
        Cycles.add(() => {
            val ProgramCounterLowByte = Cycles.getNextStoredByte()
            val ProgramCounterHighByte = pullFromStack() << 8
            programCounter = ProgramCounterHighByte | ProgramCounterLowByte
        })
        Cycles.add(() => programCounter += 1)
    }

    private def sbc(address: Option[Int]): Unit = {
        val PreviousAccumulatorNegative = (accumulator & NegativeFlagMask) > 0

        accumulator -= memory(address.get)
        accumulator -= ~(status & CarryFlagMask) & 1
        accumulator &= 0xFF

        val SetOverflow = PreviousAccumulatorNegative && (accumulator & NegativeFlagMask) == 0
        updateCarryFlag(createSignedIntFromByte(accumulator) >= 0)
        updateOverflowFlag(SetOverflow)
        updateZeroFlag(accumulator == 0)
        updateNegativeFlag(accumulator)
    }

    private def sec(): Unit = Cycles.add(() => status |= CarryFlagMask)
    private def sed(): Unit = Cycles.add(() => status |= DecimalFlagMask)
    private def sei(): Unit = Cycles.add(() => status |= InterruptDisableFlagMask)

    private def sta(address: Option[Int]): Unit = memory(address.get) = accumulator
    private def stx(address: Option[Int]): Unit = memory(address.get) = xIndex
    private def sty(address: Option[Int]): Unit = memory(address.get) = yIndex

    private def tax(): Unit = Cycles.add(() => {
        xIndex = accumulator
        updateZeroFlag(xIndex == 0)
        updateNegativeFlag(xIndex)
    })
    private def tay(): Unit = Cycles.add(() => {
        yIndex = accumulator
        updateZeroFlag(yIndex == 0)
        updateNegativeFlag(yIndex)
    })
    private def tsx(): Unit = Cycles.add(() => {
        xIndex = stackPointer
        updateZeroFlag(xIndex == 0)
        updateNegativeFlag(xIndex)
    })
    private def txa(): Unit = Cycles.add(() => {
        accumulator = xIndex
        updateZeroFlag(accumulator == 0)
        updateNegativeFlag(accumulator)
    })
    private def txs(): Unit = Cycles.add(() => stackPointer = xIndex)
    private def tya(): Unit = Cycles.add(() => {
        accumulator = yIndex
        updateZeroFlag(accumulator == 0)
        updateNegativeFlag(accumulator)
    })

}
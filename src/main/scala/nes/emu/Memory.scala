package nes.emu

class Memory {

    private var ram = Array.fill[Int](0x800)(0xEA)
    // TODO: the rest of the memory
    private var cartridge = Array.fill[Int](0xBFE0)(0)

    private var self = ram ++ cartridge


    /* --- TESTING 123 123 --- */

    self(0x0000) = 0xA0
    self(0x0001) = 0x02 // LDY #$02
    self(0x0002) = 0xB1
    self(0x0003) = 0xFF // LDA ($FC),Y
    self(0x00FF) = 0x34
    self(0x0100) = 0x12
    self(0x1236) = 0x99

    def apply(pointer: Int): Int = self(pointer)

}
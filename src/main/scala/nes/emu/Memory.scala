package nes.emu

class Memory {

    private var ram = Array.fill[Int](0x800)(0xEA)
    // TODO: the rest of the memory
    private var cartridge = Array.fill[Int](0xBFE0)(0)

    private var self = ram ++ cartridge


    /* --- TESTING 123 123 --- */

    self(0x0000) = 0xA9
    self(0x0001) = 0x02 // LDA #$02

    def apply(pointer: Int): Int = self(pointer)

}
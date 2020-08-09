package nes.emu

class Memory {

    private var ram = Array.fill(0x800)(0xEA)
    // TODO: the rest of the memory
    private var cartridge = Array.fill(0xBFE0)(0)

    private var self = ram ++ cartridge ++ Array.fill(0x10000 - 0xBFE0 - 0x800)(0xEA)


    /* --- TESTING 123 123 --- */

    self(0x0000) = 0x00 // BRK
    self(0xFFFE) = 0x34
    self(0xFFFF) = 0x12 // Interrupt vector

    def apply(pointer: Int): Int = self(pointer)
    def update(pointer: Int, replacement: Int) = self(pointer) = replacement

}
package nes.emu

class Memory {

    private var ram = Array.fill[Int](0x800)(0xEA)
    // TODO: the rest of the memory
    private var cartridge = Array.fill[Int](0xBFE0)(0)

    private var self = ram ++ cartridge


    /* --- TESTING 123 123 --- */

    self(0x0001) = 0xA9
    self(0x0002) = 0x01 // LDA #$01
    self(0x0003) = 0x24
    self(0x0004) = 0x99 // BIT $99
    self(0x0099) = 0x01

    def apply(pointer: Int): Int = self(pointer)
    def update(pointer: Int, replacement: Int) = self(pointer) = replacement

}
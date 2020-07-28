package nes.emu

class Memory {

    private var ram = Array.fill[Int](0x800)(0xEA)
    // TODO: the rest of the memory
    private var cartridge = Array.fill[Int](0xBFE0)(0)

    private var self = ram ++ cartridge


    // TESTING
    self(0x0000) = 0x6C
    self(0x0001) = 0x34
    self(0x0002) = 0x12
    self(0x1234) = 0x99

    def apply(pointer: Int): Int = self(pointer)

}
package nes.emu

class Memory {

    private var ram = new Array[Byte](0x800)
    // TODO: the rest of the memory
    private var cartridge = new Array[Byte](0xBFE0)

    private var self = ram ++ cartridge

    def apply(pointer: Int): Byte = self(pointer)

}
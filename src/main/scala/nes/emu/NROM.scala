package nes.emu

class NROM(val cartridge: Cartridge) extends Mapper {
    def getCPUAddressSpace(): Array[Int] = CPUMemoryMap ++ Array.fill(0x8000 - 0x4020)(0) ++ cartridge.PRGROM ++ cartridge.PRGROM
}
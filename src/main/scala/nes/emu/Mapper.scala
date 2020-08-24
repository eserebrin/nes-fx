package nes.emu

abstract class Mapper {
    // private val RAM = Array.fill(0x800)(0)
    // private val PPURegisters = Array.fill(8)(0)
    // private val PPURegisterMirrrors = PPURegisters * 8
    // private val APURegisters = Array.fill(0x18)(0)
    // private val APUArea = Array.fill(8)(0)

    protected val CPUMemoryMap = Array.fill(0x4020)(0)

    def getCPUAddressSpace(): Array[Int]
}

object Mapper {
    def apply(cartridge: Cartridge): Mapper = cartridge.MapperNumber match {
        case 0 => new NROM(cartridge)
    }
}
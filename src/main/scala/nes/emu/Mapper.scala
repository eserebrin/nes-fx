package nes.emu

abstract class Mapper {

    protected val CPUMemoryMap = Array.fill(0x4020)(0)
    protected var cpuAddressSpace = new Array[Int](0)

    def apply(address: Int): Int = cpuAddressSpace(address)
    def update(address: Int, value: Int): Unit = {
        cpuAddressSpace(address) = value

        // Memory mirroring
        if (address >= 0 && address <= 0x7FF) {
            for (i <- 1 to 3) {
                cpuAddressSpace(address + 0x800 * i) = value
            }
        }
        if (address >= 0x2000 && address <= 2008) {
            for (i <- 1 to 8) {
                cpuAddressSpace(address + 0x1000 * i) = value
            }
        }
    }

}

object Mapper {
    def apply(cartridge: Cartridge): Mapper = cartridge.MapperNumber match {
        case 0 => new NROM(cartridge)
    }
}
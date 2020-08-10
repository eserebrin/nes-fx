package nes.emu

import scala.collection.mutable
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.File

class Memory(cartridgeFileName: String) {

    private val CartridgeSpaceSize = 0xBFE0

    // Read ROM contents from file
    private var romContents = mutable.Buffer[Int]()
    private val CartridgeFileInstream = new BufferedInputStream(new FileInputStream(new File(cartridgeFileName)))
    while(CartridgeFileInstream.available > 0) romContents += CartridgeFileInstream.read()
    CartridgeFileInstream.close()

    private var ram = Array.fill(0x800)(0)
    private var cartridge = romContents ++ Array.fill(CartridgeSpaceSize - romContents.size)(0)
    private var self = ram ++ Array.fill(0x10000 - 0xBFE0 - 0x800)(0) ++ cartridge


    /* --- Header initialization --- */

    private val header = cartridge.take(16)

    // Determine file format is correct
    private var iNESFormat = false
    private var iNES2Format = false

    if (header(0) == 'N' && header(1) == 'E' && header(2) == 'S' && header(3) == 0x1A) iNESFormat = true
    if (iNESFormat && (header(7) & 0x0C) == 0x08) iNES2Format = true

    if (!iNESFormat && !iNES2Format) {
        println("Invalid ROM type.\niNES and NES 2.0 File Formats are supported.")
        sys.exit(1)
    }

    private val trainerArea = if ((header(6) & 2) >> 1 == 1) cartridge.slice(16, 16 + 512) else Array[Int]()

    // PRG/CHR ROM size
    val PRGROMSize = header(4) 
    val CHRROMSizeLowByte = header(5)


    def apply(pointer: Int): Int = self(pointer)
    def update(pointer: Int, replacement: Int) = self(pointer) = replacement

}
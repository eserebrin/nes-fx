package nes.emu

import scala.collection.mutable
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.File

class Memory(cartridgeFileName: String) {

    private val CartridgeSpaceSize = 0xBFE0

    /* --- Read ROM contents from file --- */

    private var romContents = mutable.Buffer[Int]()
    private val CartridgeFileInstream = new BufferedInputStream(new FileInputStream(new File(cartridgeFileName)))
    while (CartridgeFileInstream.available > 0) romContents += CartridgeFileInstream.read()
    CartridgeFileInstream.close()


    /* --- Examine Header --- */

    private val Header = romContents.take(16).toVector
    romContents --= Header

    // Determine file format is correct
    private var iNESFormat = false
    private var iNES2Format = false

    if (Header(0) == 'N' && Header(1) == 'E' && Header(2) == 'S' && Header(3) == 0x1A) iNESFormat = true
    if (iNESFormat && (Header(7) & 0x0C) == 0x08) iNES2Format = true

    if (!iNESFormat && !iNES2Format) {
        println("Invalid ROM type.\niNES and NES 2.0 File Formats are supported.")
        sys.exit(1)
    }

    /* --- Create various ROM components --- */

    private val Trainer = if ((Header(6) & 2) >> 1 == 1) romContents.take(512).toVector
                          else Vector[Int]()
    romContents --= Trainer

    /* --- PRGROM --- */

    // Detemine size of PRGROM
    private var PRGROMSize = 0
    if (Header(9) == 0xF) {
        // Use exponent representation for PRGROM size
        val Exponent = Math.pow(Header(4) & 0xFC, 2)
        val Multiplier = (Header(4) & 3) * 2 + 1
        PRGROMSize = Exponent.toInt * Multiplier
    } else {
        val HighNibble = Header(9) & 0xF
        (HighNibble << 8) | Header(4) 
    }

    private val PRGROM = romContents.take(PRGROMSize).toVector
    romContents --= PRGROM


    private val CHRROMSizeLowByte = Header(5)
    /* --- Assemble the memory! --- */

    private var ram = Array.fill(0x800)(0)

    // TODO: Mappers
    private var cartridge = Trainer ++ PRGROM

    private var self = ram ++ Array.fill(0x10000 - 0xBFE0 - 0x800)(0) ++ cartridge

    def apply(pointer: Int): Int = self(pointer)
    def update(pointer: Int, replacement: Int) = self(pointer) = replacement

}
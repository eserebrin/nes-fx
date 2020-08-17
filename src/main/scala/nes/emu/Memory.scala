package nes.emu

import scala.collection.mutable
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.File

class Memory(cartridgeFileName: String) {

    private val cartridge = createCartridge(getROMFileContents())
    private var ram = Array.fill(0x800)(0)
    private var self = ram ++ Array.fill(0x10000 - 0xBFE0 - 0x800)(0) ++ cartridge


    def apply(pointer: Int): Int = self(pointer)
    def update(pointer: Int, replacement: Int) = self(pointer) = replacement


    private def getROMFileContents(): Buffer[Int] = {
        var romContents = mutable.Buffer[Int]()
        val CartridgeFileInstream = new BufferedInputStream(new FileInputStream(new File(cartridgeFileName)))
        while (CartridgeFileInstream.available > 0) romContents += CartridgeFileInstream.read()
        CartridgeFileInstream.close()
        romContents
    }

    // TODO: Mappers
    private def createCartridge(romContents: Buffer[Int]): Array[Int] = {
        val Header = romContents.take(16).toVector
        romContents --= Header

        ensureCorrectFileFormat(Header)
        
        val Trainer = if ((Header(6) & 2) >> 1 == 1) romContents.take(512).toVector
                            else Vector[Int]()
        romContents --= Trainer

        val PRGROMSize = getROMAreaSize(lowByte = Header(4), highNibble = Header(9) & 0xF)
        val PRGROM = romContents.take(PRGROMSize).toVector
        romContents --= PRGROM

        val CHRROMSize = getROMAreaSize(lowByte = Header(5), highNibble = Header(9) & 0xF0)
        val CHRROM = romContents.take(CHRROMSize).toVector
        romContents --= CHRROM

        Trainer ++ PRGROM ++ CHRROM ++ romContents
    }

    private def ensureCorrectFileFormat(header: Vector[Int]): Unit = {
        var iNESFormat = false
        var iNES2Format = false

        if (header(0) == 'N' && header(1) == 'E' && header(2) == 'S' && header(3) == 0x1A) iNESFormat = true
        if (iNESFormat && (header(7) & 0x0C) == 0x08) iNES2Format = true

        if (!iNESFormat && !iNES2Format) {
            println("Invalid ROM type.\niNES and NES 2.0 File Formats are supported.")
            sys.exit(1)
        }
    }

    private def getROMAreaSize(lowByte: Nibble, highNibble: Int): Int = {
        if (highNibble == 0xF) {
            val Exponent = Math.pow(lowByte & 0xFC, 2)
            val Multiplier = (lowByte & 3) * 2 + 1
            Exponent.toInt * Multiplier
        }
        else (highNibble << 8) | lowByte
    }

}
package nes.emu

import scala.collection.mutable
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.File

class Cartridge(cartridgeFileName: String) {

    private val Components = getCartridgeComponents(getROMFileContents())
    val Header = Components(0)
    val MapperNumber = (Header(6) & 0xF0) >> 4
    def Trainer = Components(1)
    def PRGROM = Components(2)
    def CHRROM = Components(3)
    def MiscROM = Components(4)

    private def getROMFileContents(): mutable.Buffer[Int] = {
        var romContents = mutable.Buffer[Int]()
        val CartridgeFileInstream = new BufferedInputStream(new FileInputStream(new File(cartridgeFileName)))
        while (CartridgeFileInstream.available > 0) romContents += CartridgeFileInstream.read()
        CartridgeFileInstream.close()
        romContents
    }

    private def getCartridgeComponents(romContents: mutable.Buffer[Int]): Vector[Vector[Int]] = {
        val Header = romContents.take(16).toVector
        romContents --= Header

        ensureCorrectFileFormat(Header)
        
        val Trainer = if ((Header(6) & 2) >> 1 == 1) romContents.take(512).toVector
                      else Vector[Int]()
        romContents --= Trainer

        val PRGROMSize = getROMAreaSize(lowByte = Header(4), highNibble = Header(9) & 0xF)
        val PRGROM = romContents.take(PRGROMSize).toVector
        romContents --= PRGROM

        val CHRROMSize = getROMAreaSize(lowByte = Header(5), highNibble = (Header(9) & 0xF0) >> 4)
        val CHRROM = romContents.take(CHRROMSize).toVector
        romContents --= CHRROM

        Vector(Header, Trainer, PRGROM, CHRROM, romContents.toVector)
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

    private def getROMAreaSize(lowByte: Int, highNibble: Int): Int = {
        if (highNibble == 0xF) {
            val Exponent = Math.pow((lowByte & 0xFC) >> 2, 2)
            val Multiplier = (lowByte & 3) * 2 + 1
            Exponent.toInt * Multiplier
        }
        else ((highNibble << 8) | lowByte) * 1024 * 16
    }

}
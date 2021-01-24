package nes.emu

class PPU(memory: Mapper) {

    // PPU Register Addresses
    private val PPUCTRL = 0x2000
    private val PPUMASK = 0x2001
    private val PPUSTATUS = 0x2002
    private val OAMADDR = 0x2003 
    private val OAMDATA = 0x2004
    private val PPUSCROLL = 0x2005
    private val PPUADDR = 0x2006
    private val PPUDATA = 0x2007
    private val OAMDMA = 0x4014
}
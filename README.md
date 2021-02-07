# nes-fx
Nintendo Entertainment System emulator in Scala with ScalaFX

## Progress
- [x] CPU
- [x] Memory / ROM Support
- [ ] Mappers
  - [x] Mapper 0 (NROM)
- [ ] PPU
- [ ] Controllers
- [ ] APU

Note: Right now the emulator will just generate a file logging what the 6502 is doing.

## How to Use
1. Make sure you have Scala Build Tool installed.
2. Clone the repository to a convenient location.
3. Inside the directory at a terminal, type ```sbt run <path to .nes ROM file>```

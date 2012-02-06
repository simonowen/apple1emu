DISK=apple1emu.dsk
ROMS=apple1.rom applesoft-lite-0.4-ram.bin ehbasic.bin 65C02.rom.bin applesoft-lite-0.4.bin

.PHONY: clean

$(DISK): apple1emu.asm font.bin opdefs.inc $(ROMS)
	pyz80.py --exportfile=apple1emu.sym apple1emu.asm

font.bin: font.png png2bin.pl
	./png2bin.pl font.png 6 8

opdefs.inc: opdefs.pl opimpl.inc apple1emu.asm
	./opdefs.pl

clean:
	rm -f $(DISK) apple1emu.sym

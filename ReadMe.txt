Apple 1 Emulator for SAM Coupe (v1.2)
-------------------------------------

See website for usage: http://simonowen.com/sam/apple1emu/

---

Version 1.2 (5/9/2008)
- Fixed BRK flags (clear D, set I, N/Z set from wrong EXX set)
- Fixed Esc repeat, broken in previous version
- Added Applesoft BASIC [Lite] and Lee Davidson's Enhanced BASIC
- Updated Ken Wessen's Krusader assembler to v1.3
- First byte of emulator at &c000 appears to be ROM (for Applesoft BASIC ROM)
- Terminal output now supports lower-case characters (for Enhanced BASIC)

Version 1.1 (27/8/2008)
- Improved 65C02 core for ~20% speed boost
- Fixed broken TRB, which was masking the wrong value
- BIT #imm no longer considers setting N flag

Version 1.0 (19/3/2007)
- Initial release

---

Simon Owen
simon@simonowen.com

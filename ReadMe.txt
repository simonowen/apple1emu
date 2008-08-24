Apple 1 Emulator for SAM Coupe (v1.1)
-------------------------------------

Emulator Keys:

  Shift-Esc = Soft Reset
  Symbol-1 = 50Hz terminal
  Symbol-2 = 100Hz terminal

Symbol is mapped to Ctrl in SimCoupe, so you'll need to use Ctrl-1 and Ctrl-2.

The emulator supports a type-in feature, allowing text listings to be entered
as though they were typed.  This feature is currently available to SimCoupe
users only, and used as follows:

  - Press F4 for data import
  - Change Import Type to Main Memory
  - Select Page 6 and Offset 0
  - Click OK and select the file to import

You will need to ensure that the file is typed in the correct mode.  Hex code
listings are in the startup monitor mode.  For BASIC listings type E000R in
the monitor to run the BASIC ROM.  For 6502 ASM listings type F000R to launch
the Krusader assembler.

For further details on using the Apple 1 monitor, download the user manual:
  http://simonowen.com/sam/apple1emu/a1man.pdf (2.9MB)

---

Simon Owen <simon@simonowen.com>
http://simonowen.com/sam/apple1emu/

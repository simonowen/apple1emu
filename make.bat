@echo off

if "%1"=="clean" goto clean

opimpl.pl
pyz80.py --exportfile=apple1emu.sym apple1emu.asm
goto end

:clean
if exist apple1emu.dsk del apple1emu.dsk apple1emu.sym

:end

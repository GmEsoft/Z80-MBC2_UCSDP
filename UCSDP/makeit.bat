@echo off
set name=ucsdp
zmac\zmac %name%.asm --od %name% --oo cim,lst -c -s -g
if errorlevel 1 pause && goto :eof
copy %name%\%name%.cim autoboot.bin
if errorlevel 1 pause && goto :eof

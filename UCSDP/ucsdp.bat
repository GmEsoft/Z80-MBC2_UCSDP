set name=ucsdp
zmac %name%.asm
if errorlevel 1 pause && goto :eof
copy zout\%name%.cim autoboot.bin
if errorlevel 1 pause && goto :eof
::copy /b b80.bin+syscpm1.bin DS2N00.DSK
call Z80_MBC2 %1

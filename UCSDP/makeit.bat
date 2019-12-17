@echo off
set name=ucsdp
echo $DATE	MACRO>datetime.asm
echo 	DB	"%DATE%">>datetime.asm
echo 	ENDM>>datetime.asm
echo $TIME	MACRO>>datetime.asm
echo 	DB	"%TIME%">>datetime.asm
echo 	ENDM>>datetime.asm
zmac\zmac %name%.asm --od %name% --oo cim,lst -c -s -g
if errorlevel 1 pause && goto :eof
copy %name%\%name%.cim autoboot.bin
if errorlevel 1 pause && goto :eof

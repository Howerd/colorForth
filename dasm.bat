@echo Dis-assembling the image file .... please wait...
rem                      start address  avoid n bytes from address  filename     output filename
rem ndisasm.exe -b 32    -e 0x00000712     -k 0x00010000,0x00000FAD cf2019.img > cf2019.dasm

rem                start address   filename     output filename
rem ndisasm.exe -b 32 -e 0x00000F2D    cf2019.img > cf2019.dasm
rem ndisasm.exe -b 32 -e 0x00000712   cf2019.img > cf2019.dasm
ndisasm.exe -b 32 -e 0x00000000   cf2019.img > cf2019.dasm
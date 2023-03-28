colorForth cf2023 2023 Apr 04

colorForth is a simple dialect of Forth created by the inventor of Forth, Chuck Moore.
cf2023 is a distribution that runs on a Windows PC in bochs, or natively from a USB drive.

Double left click ( run ) go.bat to run the cf2023.img image in the bochs PC emulator. 
bochs is available from http://bochs.sourceforge.net/

It is better to copy the file cf2023.img to a USB drive, from sector 0, and boot the PC from the USB drive.
On Windows you can use Rufus.exe to do this.

Files in this archive :
c.bat       	create cf2023.img from  cf2023.nasm  and  cf2023Ref.img
cf2023.img  	the resulting executable image file
cf2023Ref.img   the latest version of cf2023.img, used to merge the font and colorForth source into the new cf2023.img
dasm.bat        disassemble a section of cf2023.img, for test only
go.bat          run the cf2023.img image in bochs
goc.bat         create cf2023.img from cf2023.nasm then run the resulting image in bochs
nasm.exe        the Netwide ASeMbler ( NASM ) program : http://www.nasm.us/
ndisasm.exe     the NASM disassembler  : http://www.nasm.us/doc/nasmdoca.html
new.bat         copies the current cf2023.img file to cf2023Ref.img. Run new.bat when you have made changes to the
                colorForth source (by typing "save" or "sa" in colorForth) and want these changes to be included when the
                system is re-created.
readme.txt      this readme file.

Short "how to" guide : the best way to use cf2023 is to open a Windows command window (cmd.exe) in the 
    directory where cf2023 is located.
    If you make changes to cf2023.nasm, run  c.bat  to create a new cf2023.img (using cf2023Ref.img, 
    a saved reference image).
    If you make changes in the colorForth editor and type "save" or "sa" you must run  new.bat  before 
    re-creating cf2023, otherwise your changes will be lost. 
    This is because  c.bat  and  goc.bat  take the colorForth source and font from the cf2023Ref.imf 
    reference image to create the new cf2023.img image.

Online help is available on comp.lang.forth newsgroup and on https://groups.google.com/forum/?hl=en-GB#!forum/color-forth.

Enjoy!

Howerd Oakford  www.inventio.co.uk  howerd@inventio.co.uk

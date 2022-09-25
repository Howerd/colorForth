# Modified from https://github.com/narke - thanks!

all:
	nasm -l cf2022.lst -o cf2022.img cf2022.nasm

ndisasm:
	#echo "start address  avoid n bytes from address  filename     output filename"
	ndisasm -b 32 -e 0x00000000   cf2022.img > cf2022.dasm

qemu:
	qemu-system-i386 -drive format=raw,file=cf2022.img,if=floppy

bochs:
	bochs -f cf2022.bxrc

clean:
	rm -f cf2022.lst cf2022.img cf2022.dasm

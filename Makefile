# Run cf2023 in Qemu, adapted from https://github.com/narke - thanks!

all:
	nasm -l cf2023.lst -o cf2023.img cf2023.nasm

ndisasm:
	#echo "start address  avoid n bytes from address  filename     output filename"
	ndisasm -b 32 -e 0x00000000   cf2023.img > cf2023.dasm

qemu:
	qemu-system-i386 -drive format=raw,file=cf2023.img,if=floppy

bochs:
	bochs -f cf2023.bxrc

clean:
	rm -f cf2023.lst cf2023.img cf2023.dasm

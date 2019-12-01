@echo off
@echo assembling  cf2019.nasm  with listing using NASM for Windows 

.\nasm.exe -O0 -l cf2019.lst -o cf2019.img cf2019.nasm

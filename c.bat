@echo off
@echo assembling  cf2023.nasm  with listing using NASM for Windows 

.\nasm.exe -O0 -l cf2023.lst -o cf2023.img cf2023.nasm

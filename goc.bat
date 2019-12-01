@echo off
REM compile using cf2019Ref.img, then run under bochs
call c.bat
cd bochs
call go.bat
cd ..


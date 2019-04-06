@echo off
setlocal

pushd "%~dp0"
    del *~
    del *.hi
    del *.o
    del *.exe
popd

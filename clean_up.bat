@echo off
setlocal

:: https://qiita.com/hkuno/items/e7fedc20a61979aa6078
:: https://qiita.com/sawa_tsuka/items/8edf3d3d33a0ae86cb5c

if "%1"=="" (
    set work_dir=%cd%

) else (
    set work_dir=%~f1
)

pushd %work_dir%
    if not "%cd%" == "%work_dir%" (
        goto :ESCAPE
    )

    del *~
    del *.hi
    del *.o
    del *.exe
    del a.out

:ESCAPE
popd


@echo off
for /f "usebackq" %%i in (`kpsewhich -var-value SELFAUTOLOC`) do (
    echo depolying aptex and aplatex ...
    copy aptex.exe "%%i" /Y
    copy aplatex.exe "%%i" /Y
    copy aplatex-dev.exe "%%i" /Y
)
@echo on
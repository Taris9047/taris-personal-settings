@echo off
call "C:\WinPython64\scripts\env.bat"
rd /S /Q .\dist .\build
del .\ffmpeg_convert_all.exe
pyinstaller --onefile .\ffmpeg_convert_all.py
xcopy /R .\dist\ffmpeg_convert_all.exe .\
rd /S /Q .\build .\dist
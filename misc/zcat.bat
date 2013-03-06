@echo off
gzip -cd "%1" > "%CD%\%~n1"

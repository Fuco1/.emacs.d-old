@echo off
gzip -cd "%1" > "%CD%\%~n1"
7z x "%CD%\%~n1"
rm "%CD%\%~n1"
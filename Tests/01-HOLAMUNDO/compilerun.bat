@echo off
copy "C:\Program Files\CodeBlocks\MinGW\bin\libst*.dll"
copy "C:\Program Files\CodeBlocks\MinGW\bin\libw*.dll"
copy "C:\Program Files\CodeBlocks\MinGW\bin\libgc*.dll"
"C:\Program Files\CodeBlocks\MinGW\bin\g++.exe" -std=c++11 main.cpp
pause
a
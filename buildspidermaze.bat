del spidermaze.prg
java -jar "c:\kickassembler\kickass.jar" spidermaze.asm 
c:\exomizer\win32\exomizer.exe sfx $4000 spidermaze.prg -o spidermaze.prg -n -Di_ram_during=$34 -q
c:\vice_runtime\x64sc.exe spidermaze.prg
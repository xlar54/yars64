del target/*.d64
del target/*.lst
del target/*.lbl
del target/yars64
64tass -a src\yars64.asm -l target\yars64.lbl -L target\yars64.lst -o target\yars64
cd target
c1541 -format "YARS64,01" d64 yars64.d64
c1541 -attach yars64.d64 -write yars64 yars64
cd ..
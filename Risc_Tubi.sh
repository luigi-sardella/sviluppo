ALS=~/Luigi/Sviluppo/Ada_LS;
QUI=~/Luigi/Sviluppo/Riscaldatori;
gnatchop -w $ALS/LS.ada;
gnatchop -w $ALS/definizioni.ada;
gnatchop -w $ALS/utilita.ada;
gnatchop -w $ALS/solve_gen.ada;
gnatchop -w $ALS/Derivate_Numeriche.ada;
gnatchop -w $ALS/vapore.ada;
gnatchop -w $ALS/diffusori.ada;
gnatchop -w $QUI/Risc_Tubi.ada;
gnatchop -w $ALS/jewl.ada;
wine gnatmake -o RiscTubi ls-risctubi;
rm ls.ads; rm ls-*.adb; rm ls-*.ads; rm jewl*.adb; rm jewl*.ads; rm *.o; rm *.ali;
#!

# cp ~/Luigi/Sviluppo/Ada_LS/LS.ada .
# cp ~/Luigi/Sviluppo/Ada_LS/definizioni.ada .
# cp ~/Luigi/Sviluppo/Ada_LS/utilita.ada .
gnatchop -w ~/Luigi/Sviluppo/Libreria/Sorgenti/LS.ada
gnatchop -w ~/Luigi/Sviluppo/Libreria/Sorgenti/definizioni.ada
gnatchop -w ~/Luigi/Sviluppo/Libreria/Sorgenti/utilita.ada
gnatchop -w prova_complex_1.ada
gprbuild spezia_1.gpr
# rm *.adb
# rm *.ads
# rm ./obj/*.*

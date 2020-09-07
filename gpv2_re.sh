Ada_LS=~/Luigi/Sviluppo/Libreria/Sorgenti;
QUI=~/Luigi/Sviluppo/GPV2/GPV2;
gnatchop -w $Ada_LS/LS.ada;
gnatchop -w $Ada_LS/definizioni.ada;
gnatchop -w $Ada_LS/solve.ada;
gnatchop -w $Ada_LS/solve_gen.ada;
gnatchop -w $Ada_LS/Derivate_Numeriche.ada;
gnatchop -w $Ada_LS/vapore.ada;
gnatchop -w $Ada_LS/utilita.ada;
gnatchop -w $QUI/gpv2_re.ada;
gnatchop -w $Ada_LS/interpolazione_curve.ada;
gnatchop -w ~/Luigi/Sviluppo/Curve_eiettori/curve_termocompressori.ada;
gnatchop -w $QUI/gpv2_comuni.ada;
gnatmake -o gpv2_re gpv2_re;
rm ls.ads; rm ls-*.adb; rm ls-*.ads; rm *.o; rm *.ali; rm gpv2_re.adb;


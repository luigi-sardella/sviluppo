Sorgenti=~/Luigi/Sviluppo/Libreria/Sorgenti;
QUI=~/Luigi/Sviluppo/GPV2;
gnatchop -w $Sorgenti/LS.ada;
gnatchop -w $Sorgenti/definizioni.ada;
gnatchop -w $Sorgenti/solve.ada;
gnatchop -w $Sorgenti/solve_gen.ada;
gnatchop -w $Sorgenti/Derivate_Numeriche.ada;
gnatchop -w $Sorgenti/vapore.ada;
gnatchop -w $Sorgenti/utilita.ada;
gnatchop -w $Sorgenti/jewl.ada;
gnatchop -w $QUI/gpv2.ada;
gnatchop -w $Sorgenti/interpolazione_curve.ada;
gnatchop -w ~/Luigi/Sviluppo/Curve_eiettori/curve_termocompressori.ada;
gnatchop -w $QUI/gpv2_comuni.ada;
wine gprbuild gpv2.gpr;
rm ls.ads; rm ls-*.adb; rm ls-*.ads; rm gpv2.adb;
rm jewl*.adb; rm jewl*.ads;

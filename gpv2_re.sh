Sorgenti=~/Luigi/Sviluppo/Libreria/Sorgenti;
QUI=~/Luigi/Sviluppo/GPV2;
gnatchop -w $Sorgenti/LS.ada;
gnatchop -w $Sorgenti/definizioni.ada;
gnatchop -w $Sorgenti/solve.ada;
gnatchop -w $Sorgenti/solve_gen.ada;
gnatchop -w $Sorgenti/Derivate_Numeriche.ada;
gnatchop -w $Sorgenti/vapore.ada;
gnatchop -w $Sorgenti/utilita.ada;
gnatchop -w $QUI/gpv2_re.ada;
gnatchop -w $Sorgenti/interpolazione_curve.ada;
gnatchop -w ~/Luigi/Sviluppo/Curve_eiettori/curve_termocompressori.ada;
gnatchop -w $QUI/gpv2_comuni.ada;
gprbuild gpv2_re.gpr;
rm ls.ads; rm ls-*.adb; rm ls-*.ads; rm gpv2_re.adb;


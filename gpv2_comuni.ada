with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics; use Ada.Numerics;
with LS.Numerico; use LS.Numerico;
with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;
with LS.Float_IO; use LS.Float_IO;
with LS.Integer_IO; use LS.Integer_IO;
with LS.Vapore; use LS.Vapore;
with LS.Solve1D; use LS.Solve1D;
with LS.Curve_Termocompressori; use LS.Curve_Termocompressori;
with LS.Utili;
with LS.Solve1D_Gen;
package LS.Gpv2_Comuni is
   type M2P2T is record Mv, Mvm, Pa, Pd, Ta : Mio_Float; end record;
   package Solve_Qui is new LS.Solve1D_Gen(Reale => Mio_Float, Param => M2P2T);
   use Solve_Qui;
   TK0 : constant Mio_Float := 273.16;
   P_At : constant Mio_Float := 101325.0;
   Rgas : constant Mio_Float := 8314.0;
   Avogadro : constant Mio_Float := 22.4;
   Vel_Att : constant Mio_Float := 40.0; -- Velocità agli attacchi
   Estrapol_1, Estrapol_2, Ottimizza : Boolean;
   Cost_Ad, P_Mol, P_Fin, Sovr, Kort1, Kort2 : Mio_Float;
   M_Asp, M_Vap, Mvm1, Mvm2, P_Int, Pd1, Ps2, Pd_Ps1, Ps_Ptr1, Pd_Ps2, Ps_Ptr2, R_Asp1, R_Asp2 : Mio_Float;
   T_Asp, T_Acq, P_Asp, P_Mot, T_Mot, Cp, Veff_D1, Veff_D2, Eq_Amb_Inc1, Eq_Amb_Inc2, Eq_Amb_Vap1, Eq_Amb_Vap2 : Mio_Float;
   Td1, Td2, Tsat_D1, Mv_Mi1, Q_Man1, Q_Man2, Tmix1, Pvap1, M_Asp1, M_Asp2, Diff_Temp, Perd_Carico : Mio_Float;
   Q_Asp1, Q_Asp2, V_Asp1, V_Asp2 : Mio_Float;     -----------------------
   Consumo_Minimo, Press_Cons_Min : Mio_Float;
   DND1, DND2, DNS1, DNS2 : Positive;
   function Da_Azzerare(Td : Mio_Float; Dati : M2P2T) return Mio_Float;
   function Calc_Td(Mv, Mvm, Pa, Pd, Ta: Mio_Float) return Mio_Float;
   function Calc_Port_Motr(Pres_Int : Mio_Float) return Mio_Float;
   procedure Stampe;
   Pochi_Dati_O_Sbagliati : exception;
end LS.Gpv2_Comuni;
package body LS.Gpv2_Comuni is
   function Da_Azzerare(Td : Mio_Float; Dati : M2P2T) return Mio_Float is
      Mv : Mio_Float := Dati.Mv;
      Mvm : Mio_Float := Dati.Mvm;
      Pd : Mio_Float := Dati.Pd;
      Pa : Mio_Float := Dati.Pa;
      Ta : Mio_Float := Dati.Ta;
   begin
      return M_Asp*Cp*Td + (Mv + Mvm)*Hv_J(Pd,Td + TK0) - M_Asp*Cp*Ta - Mv*Hv_J(Pa,Ta + TK0) - Mvm*Hv_J(P_Mot,T_Mot + TK0);
   end Da_Azzerare;
   function Calc_Td(Mv, Mvm, Pa, Pd, Ta: Mio_Float) return Mio_Float is
      Dati : M2P2T := (Mv => Mv, Mvm => Mvm, Pd => Pd, Pa => Pa, Ta => Ta);
   begin
      return Solve(Da_Azzerare'Access, Ta, T_Mot, Dati);
   end Calc_Td;
   function Calc_Port_Motr(Pres_Int : Mio_Float) return Mio_Float is
   begin
      Pd1 := Pres_Int*(1.0 + 0.5*0.01*Sovr);
      Ps2 := Pres_Int*(1.0 - 0.5*0.01*Sovr) - Perd_Carico;
      Pd_Ps1 := Pd1/P_Asp;
      Ps_Ptr1 := P_Asp/P_Mot;
      Pd_Ps2 := P_Fin/Ps2;
      Ps_Ptr2 := Ps2/P_Mot;
--    Put("PresInt : "); Put(Pres_Int, 3, 2, 0); New_Line;
      Rapp_Asp(P_Asp, Pd1, P_Mot, R_Asp1, Estrapol_1);
      R_Asp1 := R_Asp1*(1.0 - 0.01*Kort1);
      Rapp_Asp(Ps2, P_Fin, P_Mot, R_Asp2, Estrapol_2);      
      R_Asp2 := R_Asp2*(1.0 - 0.01*Kort2);
      Eq_Amb_Inc1 := M_Asp/(Correzione_PM(P_Mol)*Corr_Temp_Inc(T_Asp)); -- Equivalente in aria ambiente degli incondensabili
      Eq_Amb_Vap1 := M_Vap/(Correzione_PM(18.0)*Corr_Temp_Vap(T_Asp)); -- Equivalente in aria ambiente del vapore
      M_Asp1 := Eq_Amb_Vap1 + Eq_Amb_Inc1;
      Mvm1 := M_Asp1/R_Asp1;
      Q_Asp1 := (M_Vap/18.0 + M_Asp/P_Mol)*Avogadro*(1.0 + T_Asp/TK0)*(P_At/P_Asp)/3600.0;      ---------------
      DNS1 := LS.Utili.DND(Q_Asp1, Vel_Att);   --------------------------------
      Td1 := Calc_Td(M_Vap, Mvm1, P_Asp, Pd1, T_Asp);
      Q_Man1 := ((M_Vap + Mvm1)/18.0 + M_Asp/P_Mol)*Avogadro*(1.0 + Td1/TK0)*(P_At/Pd1)/3600.0;
      DND1 := LS.Utili.DND(Q_Man1, Vel_Att);
      Veff_D1 := Q_Man1*4.0/(Pi*(0.001*Mio_Float(DND1))**2);
      V_Asp1 := Q_Asp1*4.0/(Pi*(0.001*Mio_Float(DNS1))**2);     ---------------------------
      Tsat_D1 := Tem_Sat(Pd1) - TK0; -- Temperatura di saturazione corrispondente alla pressione di mandata
      Tmix1 := (if Diff_Temp = 0.0 then 0.5*(T_Acq + Tsat_D1) else T_Acq + Diff_Temp); -- Temperatura all'uscita dal condensatore
      Pvap1 := Pres_Sat(Tmix1 + TK0); -- .. e corrispondente pressione parziale del vapore
      Mv_Mi1 := 18.0*Pvap1/(P_Mol*(Ps2 - Pvap1)); -- Rapporto fra la massa di vapore trascinato e quella incondensabile
      Eq_Amb_Inc2 := M_Asp/(Correzione_PM(P_Mol)*Corr_Temp_Inc(Tmix1)); -- Equivalente in aria ambiente degli incondensabili
      Eq_Amb_Vap2 := Mv_Mi1*M_Asp/(Correzione_PM(18.0)*Corr_Temp_Vap(Tmix1)); -- Equivalente in aria ambiente del vapore
      M_Asp2 := Eq_Amb_Vap2 + Eq_Amb_Inc2;
      Mvm2 := M_Asp2/R_Asp2;
      Td2 := Calc_Td(Mv_Mi1*M_asp, Mvm2, Ps2, P_Fin, Tmix1);
      Q_Asp2 := (Mv_Mi1*M_Asp/18.0 + M_Asp/P_Mol)*Avogadro*(1.0 + Tmix1/TK0)*(P_At/Ps2)/3600.0;      ---------------
      Q_Man2 := ((Mv_Mi1*M_asp + Mvm2)/18.0 + M_Asp/P_Mol)*Avogadro*(1.0 + Td2/TK0)*(P_At/P_Fin)/3600.0;
      DND2 := LS.Utili.DND(Q_Man2, Vel_Att);
      DNS2 := LS.Utili.DND(Q_Asp2, Vel_Att);
      V_Asp2 := Q_Asp2*4.0/(Pi*(0.001*Mio_Float(DNS2))**2);     ---------------------------
      Veff_D2 := Q_Man2*4.0/(Pi*(0.001*Mio_Float(DND2))**2);
      return Mvm1 + Mvm2;
   end Calc_Port_Motr;
   procedure Stampe is    
   begin
      New_Line;
      Put("----------------------------"); New_Line;
      Put("GRUPPO PER VUOTO A DUE STADI"); New_Line;
      Put("----------------------------"); New_Line;
      Put("Pressione di aspirazione : "); Put(0.01*P_Asp, 3, 2, 0); Put(" mbar"); New_Line;
      Put("Temperatura all' aspirazione : "); Put(T_Asp, 3, 2, 0); Put(" gradi C"); New_Line;
      Put("Pressione di mandata : "); Put(0.01*P_Fin, 3, 2, 0); Put(" mbar"); New_Line;
      Put("Pressione del vapore motore : "); Put(1.0E-05*P_Mot, 3, 2, 0); Put(" bara"); New_Line;
      declare
         Numero_Formattato : String(1..10);
         Gradi_A_Satur : Mio_Float := Tem_Sat(P_Mot)  - TK0;
      begin
         Put(Numero_Formattato, Gradi_A_Satur, 4, 0);
         Put("Temperatura del vapore motore : "); Put(T_Mot, 3, 2, 0); Put(" gradi C; saturo a " & Numero_Formattato & " gradi C");
         New_Line;
      end;
      Put("Portata di incondensabili : "); Put(M_Asp, 3, 2, 0); Put(" kg/h"); New_Line;
      Put("Peso molecolare medio degli incondensabili : "); Put(P_Mol, 3, 2, 0); Put(" kg/kmole"); New_Line;
      Put(".. e loro coefficiente adiabatico : "); Put(Cost_Ad, 3, 2, 0); New_Line;
      Put("Portata di vapore trascinato : "); Put(M_Vap, 3, 2, 0); Put(" kg/h"); New_Line;
      Put("---------------------------------------------------"); New_Line;
      if Ottimizza then
         Put("---------------------------------------------------------------"); New_Line;
         Put("Calcola la pressione intermedia per il minimo consumo di vapore"); New_Line;
         Put("---------------------------------------------------------------"); New_Line;
      else
         Put("----------------------"); New_Line;
      end if;
      Put("--- PRIMO EIETTORE ---"); New_Line;
      Put("Pressione di mandata = "); Put(0.01*Pd1, 3, 2, 0); Put(" mbar"); New_Line;
      Put(" Pd / Ps = "); Put(Pd_Ps1, 3, 3, 0); New_Line;
      Put(" Ps / Ptr = "); Put(Ps_Ptr1, 3, 3, 0); New_Line;
      if Estrapol_1 Then
         Put("-------------------------------------------------------"); New_Line;
         Put("ATTENZIONE, estrapolazione delle curve, risultati dubbi"); New_Line;
         Put("-------------------------------------------------------"); New_Line;
      end if;
      Put("Rapporto d'aspirazione dalle curve Korting = "); Put(R_Asp1/(1.0 - 0.01*Kort1), 3, 3, 0); New_Line;   
      Put("Peggioramento percentuale rispetto alla Korting = "); Put(Kort1, 3, 1, 0); New_Line;
      Put("Rapporto d'aspirazione corretto = "); Put(R_Asp1, 3, 3, 0); New_Line;
      Put("Portata di aria a 20 gradi equiv. agli incondensabili = "); Put(Eq_Amb_Inc1, 3, 2, 0); Put(" kg/h"); New_Line;
      Put("Portata di aria a 20 gradi equivalente al vapore = "); Put(Eq_Amb_Vap1, 3, 2, 0); Put(" kg/h"); New_Line;
      Put("Portata aspirata equivalente (incondensabili + vapore) = "); Put(M_Asp1, 3, 2, 0); Put(" kg/h"); New_Line;
      Put("Portata di vapore motore = "); Put(Mvm1, 3, 2, 0); Put(" kg/h"); New_Line;
      Put("Temperatura alla mandata = "); Put(Td1, 3, 1, 0); Put(" gradi C"); New_Line;
      Put("Attacco alla mandata DN"); Put(DND1,3); Put(" per una velocita' di ");
      Put(Veff_D1, 3, 2, 0); Put(" m/s"); New_Line;
      Put("Attacco all'aspirazione DN"); Put(DNS1,3); Put(" per una velocita' di ");
      Put(V_Asp1, 3, 2, 0); Put(" m/s"); New_Line;
      Put("--- INTERCONDENSATORE  ---"); New_Line;
      Put("Temperatura dell'acqua di raffreddamento : "); Put(T_Acq, 3, 1, 0); Put(" gradi C"); New_Line;
      Put("Temperatura di saturazione corrispondente alla pressione di mandata = ");
      Put(Tsat_D1, 3, 2, 0); Put(" gradi C"); New_Line;
      Put("Differenza di temperature : "); Put(Diff_Temp, 3, 2, 0); Put(" gradi C"); New_Line;
      Put("Temperatura all'uscita dal condensatore = "); Put(Tmix1, 3, 2, 0); Put(" gradi C"); New_Line;
      Put(".. corrispondente pressione parziale del vapore = "); Put(0.01*Pvap1, 3, 2, 0); Put(" mbar"); New_Line;
      Put("Rapporto fra le masse di vapore e di incondensabili uscenti dal condensatore = ");
      Put(Mv_Mi1, 3, 2, 0); Put(" kg vap / kg inc"); New_Line;   ----------------------
      Put("Massa di vapore uscente dal condensatore = ");
      Put(Mv_Mi1*M_Asp, 3, 2, 0); Put_Line(" kg/ora");
      Put("--- SECONDO EIETTORE ---"); New_Line;
      Put(" Pd / Ps = "); Put(Pd_Ps2, 3, 3, 0); New_Line;
      Put(" Ps / Ptr = "); Put(Ps_Ptr2, 3, 3, 0); New_Line;
      if Estrapol_2 Then
         Put("-------------------------------------------------------"); New_Line;
         Put("ATTENZIONE, estrapolazione delle curve, risultati dubbi"); New_Line;
         Put("-------------------------------------------------------"); New_Line;
      end if;
      Put("Rapporto d'aspirazione dalle curve Korting = "); Put(R_Asp2/(1.0 - 0.01*Kort2), 3, 3, 0); New_Line;   
      Put("Peggioramento percentuale rispetto alla Korting = "); Put(Kort2, 3, 1, 0); New_Line;
      Put("Rapporto d'aspirazione corretto = "); Put(R_Asp2, 3, 3, 0); New_Line;
      Put("Portata di aria a 20 gradi equiv. agli incondensabili = "); Put(Eq_Amb_Inc2, 3, 2, 0); Put(" kg/h"); New_Line;
      Put("Portata di aria a 20 gradi equivalente al vapore = "); Put(Eq_Amb_Vap2, 3, 2, 0); Put(" kg/h"); New_Line;
      Put("Portata aspirata equivalente (incondensabili + vapore) = "); Put(M_Asp2, 3, 2, 0); Put(" kg/h"); New_Line;
      Put("Portata di vapore motore = "); Put(Mvm2, 3, 2, 0); Put(" kg/h"); New_Line;
      Put("Temperatura alla mandata = "); Put(Td2, 3, 1, 0); Put(" gradi C"); New_Line;
      Put("Attacco alla mandata DN"); Put(DND2,3); Put(" per una velocita' di ");
      Put(Veff_D2, 3, 2, 0); Put(" m/s"); New_Line;
      Put("Attacco all'aspirazione DN"); Put(DNS2,3); Put(" per una velocita' di ");
      Put(V_Asp2, 3, 2, 0); Put(" m/s"); New_Line;
      Put("Consumo totale di vapore motore = "); Put(Mvm1 + Mvm2, 3, 2, 0); Put(" kg/h"); New_Line;
   end Stampe;
end LS.Gpv2_Comuni;

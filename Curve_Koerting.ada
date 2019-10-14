with Ada.Numerics; use Ada.Numerics;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with LS.Integer_IO; use LS.Integer_IO;
with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;
with LS.Utili; use LS.Utili;
with JEWL.Simple_Windows; use JEWL.Simple_Windows;
with LS.Curve_Termocompressori; use LS.Curve_Termocompressori;
with LS.Vapore;
with LS.Solve1D; 
with LS.Flusso_Isentropico; 
procedure Koerting is
   H_Win : constant Positive := 400; -- Altezza della finestra
   W_Win : constant Positive := 1000; -- Larghezza della finestra
   X_Lab : constant Positive := 10;  -- Ascissa dell'inizio delle etichette
   X_Edit : constant Positive := 350;--    ""          ""   dei campi di introduzione
   W_Lab : constant Positive := 300; -- Larghezza delle etichette
   W_Edit : constant Positive := 100; --   ""      dei campi
   Alt : constant Positive := 20;    -- Altezza dei campi
   Y_Ini : constant Positive := 10;      -- Ordinata iniziale delle etichette
   Y_Ini_Ris : constant Positive := 400; --    ""       ""    dei risultati
   DY : constant Positive := 25;         -- Spostamenti verso il basso
   Fin : Frame_Type := Frame(H_Win,W_Win,"Curve Koerting per termocompressori a getto di vapore",'Q');
   L_Pasp : Label_Type := Label(Fin,(X_Lab,Y_Ini),W_Lab,Alt,"Pressione di aspirazione, bar");
   E_Pasp : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini),W_Edit,Alt,"1.0");
   L_Tasp : Label_Type := Label(Fin,(X_Lab,Y_Ini+DY),W_Lab,Alt,"Temperatura di aspirazione, gradi C");
   E_Tasp : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+DY),W_Edit,Alt,"23.89");
   L_Pmot: Label_Type := Label(Fin,(X_Lab,Y_Ini+2*DY),W_Lab,Alt,"Pressione motrice, bar");
   E_Pmot : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+2*DY),W_Edit,Alt,"2.0");
   L_Tmot: Label_Type := Label(Fin,(X_Lab,Y_Ini+3*DY),W_Lab,Alt,"Temp. vapore motore (se 0: saturo), gradi C");
   E_Tmot : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+3*DY),W_Edit,Alt,"0.0");
   L_Pman : Label_Type := Label(Fin,(X_Lab,Y_Ini+4*DY),W_Lab,Alt,"Pressione di mandata, bar");
   E_Pman : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+4*DY),W_Edit,Alt,"1.3");
   L_Mvap : Label_Type := Label(Fin,(X_Lab,Y_Ini+5*DY),W_Lab,Alt,"Portata di vapore aspirato, kg/h");
   E_Mvap : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+5*DY),W_Edit,Alt,"0.0");
   L_Minc: Label_Type := Label(Fin,(X_Lab,Y_Ini+6*DY),W_Lab,Alt,"Portata di incondensabili aspirati, kg/h");
   E_Minc : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+6*DY),W_Edit,Alt,"1.0");
   L_Zinc : Label_Type := Label(Fin,(X_Lab,Y_Ini+7*DY),W_Lab,Alt,"Peso molecolare degli incondensabili");
   E_Zinc : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+7*DY),W_Edit,Alt,"29.0");
   L_Ginc : Label_Type := Label(Fin,(X_Lab,Y_Ini+8*DY),W_Lab,Alt,"Coeff. adiabatico degli incondensabili");
   E_Ginc : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+8*DY),W_Edit,Alt,"1.4");
   L_Pegg : Label_Type := Label(Fin,(X_Lab,Y_Ini+9*DY),W_Lab,Alt,"Peggioramento % risp. a Korting");
   E_Pegg : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+9*DY),W_Edit,Alt,"10.0");
   L_Vatt : Label_Type := Label(Fin,(X_Lab,Y_Ini+10*DY),W_Lab,Alt,"Velocita' agli attacchi, m/s");
   E_Vatt : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+10*DY),W_Edit,Alt,"60.0");
   L_Pdps1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris),W_Lab,Alt,"PD / PS",Right);
   L_Pdps2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris),W_Lab,Alt,"");
   L_Psptr1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+DY),W_Lab,Alt,"  PS / PTR",Right);
   L_Psptr2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+DY),W_Lab,Alt,"");
   L_Rasp1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+2*DY),W_Lab,Alt," Rapporto di aspirazione dalle curve",Right);
   L_Rasp2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+2*DY),W_Lab,Alt,"");
   L_Rasc1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+3*DY),W_Lab,Alt," Rapporto di aspirazione corretto",Right);
   L_Rasc2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+3*DY),W_Lab,Alt,"");
   L_Pmot1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+4*DY),W_Lab,Alt," Portata motrice, kg/h",Right);
   L_Pmot2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+4*DY),W_Lab,Alt,"");
   L_Pmol1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+5*DY),W_Lab,Alt," Correz. P. Mol. incondensabili",Right);
   L_Pmol2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+5*DY),W_Lab,Alt,"");
   L_TInc1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+6*DY),W_Lab,Alt," Correz. temp. incondensabili",Right);
   L_TInc2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+6*DY),W_Lab,Alt,"");
   L_TVap1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+7*DY),W_Lab,Alt," Correz. temp. vapore",Right);
   L_TVap2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+7*DY),W_Lab,Alt,"");
   L_Tman1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+8*DY),W_Lab,Alt," Temperatura alla mandata",Right);
   L_Tman2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+8*DY),W_Lab,Alt,"");
   L_Dgol1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+9*DY),W_Lab,Alt," Diametro di gola ugello, mm",Right);
   L_Dgol2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+9*DY),W_Lab,Alt,"");
   L_Dboc1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+10*DY),W_Lab,Alt," Diametro di bocca ugello, mm",Right);
   L_Dboc2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+10*DY),W_Lab,Alt,"");
   L_AttM1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+11*DY),W_Lab,Alt," Diametro alla mandata",Right);
   L_AttM2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+11*DY),W_Lab,Alt,"");
   L_Vman1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+12*DY),W_Lab,Alt," Velocita' alla mandata, m/s",Right);
   L_Vman2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+12*DY),W_Lab,Alt,"");
   L_AttA1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+13*DY),W_Lab,Alt," Diametro all'aspirazione",Right);
   L_AttA2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+13*DY),W_Lab,Alt,"");
   L_Vasp1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+14*DY),W_Lab,Alt," Velocita' all'aspirazione, m/s",Right);
   L_Vasp2 : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+14*DY),W_Lab,Alt,"");
   L_Saturo: Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+15*DY),W_Lab,Alt,"");
   L_Limiti: Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+16*DY),W_Lab,Alt,"");
   L_Attenz: Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+17*DY),W_Lab,Alt,"");
   Calc : Button_Type := Button(Fin,(X_Lab,Y_Ini_Ris+18*DY),2*W_Edit,Alt,"Calcola",'X');
   Testo : String(1..10);
   P_Asp, P_Mot, P_Man, Q_Man, Q_Asp, V_Man, V_Asp, V_Att, T_Asp, T_Mot, M_Vap, M_Mot, M_Inc, Z_Inc, G_Inc, Pegg, 
     Rapporto, Rapp_Vap, Rapp_Inc, T_Man : Mio_Float;
   Diam_Gola, Diam_Bocca : Mio_Float;
   Estrapol : Boolean;
   function Da_Azzerare(Td : Mio_Float) return Mio_Float is
      T0 : Mio_Float := 273.15;
      Cp : Mio_Float := (G_Inc/(G_Inc - 1.0))*8314.0/Z_Inc;
   begin
      return M_Inc*Cp*Td + (M_Vap + M_Mot)*LS.Vapore.Hv_J(1.0E5*P_Man,Td+T0) - M_Inc*Cp*T_Asp - 
             M_Vap*LS.Vapore.Hv_J(1.0E5*P_Asp,T_Asp+T0) - M_Mot*LS.Vapore.Hv_J(1.0E5*P_Mot,T_Mot);
   end Da_Azzerare;
   procedure Diametri_Ugello(Gamma : in Mio_Float; Diam_Gola, Diam_Bocca : out Mio_Float) is
      use LS.Flusso_Isentropico;
      R_Vap : Mio_Float := 8314.0/18.0;
      Tem_Gola : Mio_Float := T_Mot/M_T0T(1.0,Gamma);
      Pre_Gola : Mio_Float := 1.0E5*P_Mot/M_P0P(1.0,Gamma);
      Den_Gola : Mio_Float := Pre_Gola/(R_Vap*Tem_Gola);
      Vel_Gola : Mio_Float := 1.0*Vel_Suono(Tem_Gola, R_Vap, Gamma);
      Mach_Bocca : Mio_Float := P0P_M(P_Mot, P_Asp, Gamma);
      Tem_Bocca : Mio_Float := T_Mot/M_T0T(Mach_Bocca, Gamma);
      Vel_Bocca : Mio_Float := Mach_Bocca*Vel_Suono(Tem_Bocca, R_Vap, Gamma);
      Den_Bocca : Mio_Float := 1.0E5*P_Asp/(R_Vap*Tem_Bocca);
   begin
      Diam_Gola := Dia_Cer(M_Mot/(3600.0*Den_Gola*Vel_Gola));
      Diam_Bocca := Dia_Cer(M_Mot/(3600.0*Den_Bocca*Vel_Bocca));
   end Diametri_Ugello;
   Avogadro : Mio_Float := 22.4;
   Pres_Atm : Mio_Float := 101325.0;
   Saturo : Boolean := False;
begin
   while Valid(Fin) loop
      begin
         case Next_Command is
            when 'X' =>
               P_Asp := Mio_Float'Value(Get_Text(E_Pasp));
               P_Mot := Mio_Float'Value(Get_Text(E_Pmot));
               T_Mot := Mio_Float'Value(Get_Text(E_Tmot)) + 273.16;
               Saturo := T_Mot < LS.Vapore.Tem_Sat(1.0E5*P_Mot); -- se T_Mot è 0 o comunque < T_Sat, il vapore è considerato saturo
               T_Mot := (if Saturo then LS.Vapore.Tem_Sat(1.0E5*P_Mot) else T_Mot);   
               P_Man := Mio_Float'Value(Get_Text(E_Pman));
               T_Asp := Mio_Float'Value(Get_Text(E_Tasp));
               M_Vap := Mio_Float'Value(Get_Text(E_Mvap));
               M_Inc := Mio_Float'Value(Get_Text(E_Minc));
               Z_Inc := Mio_Float'Value(Get_Text(E_Zinc));
               G_Inc := Mio_Float'Value(Get_Text(E_Ginc));
               Pegg := Mio_Float'Value(Get_Text(E_Pegg));
               V_Att := Mio_Float'Value(Get_Text(E_Vatt));
               Rapp_Asp(P_Asp, P_Man, P_Mot, Rapporto, Estrapol);
               if Estrapol then
                  Set_Text(L_Limiti, "ATTENZIONE, siamo fuori dai limiti delle curve");
               else 
                  Set_Text(L_Limiti, " ");                
               end if;
               if Saturo then
                  Set_Text(L_Saturo, "Vapore motore saturo");
               else 
                  Set_Text(L_Saturo, " ");                
               end if;
               Rapp_Vap := Rapporto*(M_Vap/(M_Vap + M_Inc))*Correzione_PM(18.0)*Corr_Temp_Vap(T_Asp)*(1.0 - Pegg/100.0);
               Rapp_Inc := Rapporto*(M_Inc/(M_Vap + M_Inc))*Correzione_PM(Z_Inc)*Corr_Temp_Inc(T_Asp)*(1.0 - Pegg/100.0);
               Put(Testo, P_Man/P_Asp, 3, 0);
               Set_Text(L_Pdps2, Testo);
               Put(Testo, P_Asp/P_Mot, 3, 0);
               Set_Text(L_Psptr2, Testo);
               Put(Testo, Rapporto, 3, 0);
               Set_Text(L_Rasp2, Testo);
               Put(Testo, Rapp_Vap + Rapp_Inc, 3, 0);
               Set_Text(L_Rasc2, Testo);
               M_Mot := (M_Vap + M_Inc)/(Rapp_Vap + Rapp_Inc);
               Put(Testo, M_Mot, 3, 0);
               Set_Text(L_Pmot2, Testo);
               Put(Testo, Correzione_PM(Z_Inc), 3, 0);
               Set_Text(L_Pmol2, Testo);
               Put(Testo, Corr_Temp_Inc(T_asp), 3, 0);
               Set_Text(L_TInc2, Testo);
               Put(Testo, Corr_Temp_Vap(T_Asp), 3, 0);
               Set_Text(L_TVap2, Testo);
               T_Man := LS.Solve1d.Solve(Da_Azzerare'Access, T_Asp);
               Put(Testo, T_Man, 3, 0);
               Set_Text(L_Tman2, Testo);
                  Diametri_Ugello((if Saturo then 1.135 else 1.33), Diam_Gola, Diam_Bocca);
               Put(Testo, 1000.0*Diam_Gola, 3, 0);
               Set_Text(L_Dgol2, Testo);
               Put(Testo, 1000.0*Diam_Bocca, 3, 0);
               Set_Text(L_Dboc2, Testo);
               Q_Man := ((M_Vap + M_Mot)/18.0 + M_Inc/Z_Inc)*Avogadro*
		 ((T_Man + 273.15)/273.15)*(Pres_Atm/(1.0E5*P_Man))/3600.0;
               Put(Testo, DND(Q_Man, V_Att));
               Set_Text(L_AttM2, "DN" & Testo(8..10));
               V_Man := Q_Man*4.0/(Pi*(0.001*Mio_Float(DND(Q_Man, V_Att)))**2);
               Put(Testo, V_Man, 3, 0);
               Set_Text(L_Vman2, Testo);
               Q_Asp := (M_Vap/18.0 + M_Inc/Z_Inc)*Avogadro*
		 ((T_Asp + 273.15)/273.15)*(Pres_Atm/(1.0E5*P_Asp))/3600.0;
               Put(Testo, DND(Q_Asp, V_Att));
               Set_Text(L_AttA2, "DN" & Testo(8..10));
               V_Asp := Q_Asp*4.0/(Pi*(0.001*Mio_Float(DND(Q_Asp, V_Att)))**2);
               Put(Testo, V_Asp, 3, 0);
               Set_Text(L_Vasp2, Testo);
	    when others => null;
         end case;
      exception
         when others => Set_Text(L_Attenz, "Attenzione, qualcosa non va");
      end;
   end loop;
end Koerting;

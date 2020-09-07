with Ada.Numerics; use Ada.Numerics;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Calendar; use Ada.Calendar;
with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with LS.Integer_IO; use LS.Integer_IO;
with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;
with LS.Utili; use LS.Utili;
with LS.Vapore;
with LS.Pdc_Diff;
with JEWL.Simple_Windows; use JEWL.Simple_Windows;
package LS.Risc_Tubi is
   N_Sez, Nu_Port, DN : Positive;
   Dia_Fori,Dia_Int,Num_Fori,Angolo,Port_Acq,Temp_Acq,Pres_Acq,Pres_Vap,Temp_Vap,Vel_Vap,
     Area_Sez, BetaD, DN_Vap, Q_Vap, Dens, Cal_Spec : Mio_Float;
   type Valori_Calcolati is record
      Riscaldamento, Pres_Finale, Temp_Finale, Consumo : Mio_Float;
   end record;
   type Diam is record
      Interno, Dei_Fori : Mio_Float;
   end record;
   function Calcola(Pre_Vap : Mio_Float) return Valori_Calcolati;
   function Diametri(DN : Positive) return Diam;
   procedure Esegui;
   Pressione_Acqua_Negativa, Pressione_Vapore_Insufficiente,
     Margine_Sulla_Saturazione_Superato : exception;
   Dens_Vap : Mio_Float;
end LS.Risc_Tubi;
package body LS.Risc_Tubi is
   function Diametri(DN : Positive) return Diam is
      Diametro : Diam;
   begin
      case DN is
         when 20 => Diametro.Interno := 10.0; Diametro.Dei_Fori := 2.5;
         when 25 => Diametro.Interno := 12.0; Diametro.Dei_Fori := 3.0;
         when 32 => Diametro.Interno := 16.0; Diametro.Dei_Fori := 4.0;
         when 40 => Diametro.Interno := 20.0; Diametro.Dei_Fori := 5.0;
         when 50 => Diametro.Interno := 26.0; Diametro.Dei_Fori := 6.5;
         when 65 => Diametro.Interno := 35.0; Diametro.Dei_Fori := 8.0;
         when 80 => Diametro.Interno := 42.0; Diametro.Dei_Fori := 10.0;
         when 100 => Diametro.Interno := 63.0; Diametro.Dei_Fori := 13.0;
         when 125 => Diametro.Interno := 80.0; Diametro.Dei_Fori := 16.0;
         when 150 => Diametro.Interno := 100.0; Diametro.Dei_Fori := 19.0;
         when 200 => Diametro.Interno := 125.0; Diametro.Dei_Fori := 25.0;
         when 250 => Diametro.Interno := 150.0; Diametro.Dei_Fori := 30.0;
         when 300 => Diametro.Interno := 200.0; Diametro.Dei_Fori := 35.0;
         when 350 => Diametro.Interno := 250.0; Diametro.Dei_Fori := 40.0;
         when 400 => Diametro.Interno := 270.0; Diametro.Dei_Fori := 45.0;
         when 450 => Diametro.Interno := 300.0; Diametro.Dei_Fori := 50.0;
         when 500 => Diametro.Interno := 350.0; Diametro.Dei_Fori := 55.0;
         when others => null;
      end case;
      return Diametro;
   end Diametri;
   function Calcola(Pre_Vap : Mio_Float) return Valori_Calcolati is
--    Dens : Mio_Float := 1000.0; -- (992.2 + 958.4)/2.0; -- media dei valori fra 40 e 100 gradi
--    Cal_Spec : Mio_Float := 4186.0;
      Coseno : Mio_Float := Cos(Angolo*Pi/180.0);
      Hv : Mio_Float := LS.Vapore.Hv_J(Pre_Vap,Temp_Vap);
      ArDN : Mio_Float := Ar_Cer(Mio_Float(DN)/1000.0);
      Ar_Gol : Mio_Float := Ar_Cer(Dia_Int/1000.0);
      Afori : Mio_Float := Ar_Cer(Dia_fori/1000.0) * Num_Fori;
      Pres : Mio_Float := Pres_Acq - 0.5*Dens*Port_Acq**2*(1.0/Ar_Gol**2 - 1.0/ArDN**2);
      Mass : Mio_Float := Dens*Port_Acq;
      Mass_Ini : Mio_Float := Mass;
      D_Mass, Vel : Mio_Float;
      Ris : Valori_Calcolati;
      function Visc_D_Acqua(Temp : Mio_Float) return Mio_Float is
      begin
         return 2.2097E-3 + 1.4275E-5*Temp - 0.33539E-3*Sqrt(Temp); -- Temperatura in centigradi, da 5 a 100
      end Visc_D_Acqua;
      Perd_Carico : Mio_Float;
   begin
      Area_Sez := ArDN;
      if Pres <= 0.0 then raise Pressione_Acqua_Negativa; end if;
      Put("Pressioni"); New_Line;
      for I in 1..N_Sez loop
         if Pre_Vap <= Pres then raise Pressione_Vapore_Insufficiente; end if;
         D_Mass := Afori*0.61*(1.0 - (1.0 - Pres/Pre_Vap)*0.41/1.135)
           * Sqrt(2.0*Dens_Vap*(Pre_Vap - Pres));
         Vel := D_Mass/(Afori*Dens_Vap);
         Pres := Pres + D_Mass*(Coseno*Vel - (2.0*Mass + D_Mass)/(Dens*Ar_Gol))/Ar_Gol; -- controllato
         Put(Pres/1.0E5); Put("  ");
         Mass := Mass + D_Mass;
         Ris.Riscaldamento := (Mass_Ini*Cal_Spec*Temp_Acq + (Mass - Mass_Ini)*Hv) /
                              (Cal_Spec*Mass) - Temp_Acq;
         if LS.Vapore.Pres_Sat(Ris.Riscaldamento + Temp_Acq + 273.16) >= Pres then
            raise Margine_Sulla_Saturazione_Superato;
         end if;
      end loop;
      New_Line;
      Ris.Consumo := Mass - Mass_Ini;
      Ris.Temp_Finale := Ris.Riscaldamento + Temp_Acq;
      Perd_Carico := LS.Pdc_Diff.Perdita_Carico(Semi_Ang => BetaD, Area_In => Ar_Gol, Area_Fin => ArDN, Dens => Dens, 
                                                Visc_D => Visc_D_Acqua(Ris.Temp_Finale), Portata  =>  Mass);
      Ris.Pres_Finale := Pres + 0.5*Mass**2*(1.0/Ar_Gol**2 - 1.0/ArDN**2)/Dens - Perd_Carico;
      return Ris;
   end Calcola;
   procedure Esegui is
      H_Win : constant Positive := 500; -- Altezza della finestra
      W_Win : constant Positive := 700; -- Larghezza della finestra
      X_Lab : constant Positive := 10;  -- Ascissa dell'inizio delle etichette
      X_Edit : constant Positive := 200;--    ""          ""   dei campi di introduzione
      W_Lab : constant Positive := 200; -- Larghezza delle etichette
      W_Edit : constant Positive := 50; --   ""      dei campi
      Alt : constant Positive := 20;    -- Altezza dei campi
      Y_Ini : constant Positive := 20;      -- Ordinata iniziale delle etichette
      Y_Ini_Ris : constant Positive := 430; --    ""       ""    dei risultati
      DY : constant Positive := 30;         -- Spostamenti verso il basso
      X_Cons : Positive := 250;
      X_DN : Positive := 400;
      Fin : Frame_Type := Frame(H_Win,W_Win,"Riscaldatore per tubazioni",'Q');
      L_Prt_Acq : Label_Type := Label(Fin,(X_Lab,Y_Ini),W_Lab,Alt,"Portata d'acqua, mc/h");
      Prt_Acq : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini),W_Edit,Alt,"40.0");
      L_Prs_Vap: Label_Type := Label(Fin,(X_Lab,Y_Ini+DY),W_Lab,Alt,"Pressione del vapore, bara");
      Prs_Vap : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+DY),W_Edit,Alt,"2.5");
      L_Tem_Vap: Label_Type := Label(Fin,(X_Lab,Y_Ini+2*DY),W_Lab,Alt,"Temperatura del vapore, °C");
      Tem_Vap : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+2*DY),W_Edit,Alt,"Saturo");
      L_Prs_Acq : Label_Type := Label(Fin,(X_Lab,Y_Ini+3*DY),W_Lab,Alt,"Pressione dell'acqua, bara");
      Prs_Acq : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+3*DY),W_Edit,Alt,"1.0");
      L_Tmp_Acq: Label_Type := Label(Fin,(X_Lab,Y_Ini+4*DY),W_Lab,Alt,"Temperatura dell'acqua, °C");
      Tmp_acq : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+4*DY),W_Edit,Alt,"20.0");
      L_Cons : Label_Type := Label(Fin,(X_Cons+10,Y_Ini+4*DY),W_Lab,Alt,"Predefiniti:");
      L_Diagola : Label_Type := Label(Fin,(X_Lab,Y_Ini+5*DY),W_Lab,Alt,"Diametro della gola, mm");
      Diagola : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+5*DY),W_Edit,Alt,"42.0");
      L_gola : Label_Type := Label(Fin,(X_Cons+10,Y_Ini+5*DY),W_Lab/2,Alt,"42.0");
      L_Diafori : Label_Type := Label(Fin,(X_Lab,Y_Ini+6*DY),W_Lab,Alt,"Diametro dei fori");
      Diafori : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+6*DY),W_Edit,Alt,"10.0");
      L_For : Label_Type := Label(Fin,(X_Cons+10,Y_Ini+6*DY),W_Lab/2,Alt,"10.0");
      L_Sezioni : Label_Type := Label(Fin,(X_Lab,Y_Ini+7*DY),W_Lab,Alt,"Numero delle sezioni");
      Sezioni : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+7*DY),W_Edit,Alt,"5");
      L_Fori : Label_Type := Label(Fin,(X_Lab,Y_Ini+8*DY),W_Lab,Alt,"Fori per ogni sezione");
      Fori : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+8*DY),W_Edit,Alt,"4");
      L_Angol : Label_Type := Label(Fin,(X_Lab,Y_Ini+9*DY),W_Lab,Alt,"Angolo dei fori risp. all'asse, grd");
      Angol : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+9*DY),W_Edit,Alt,"45.0");
      L_BetaD : Label_Type := Label(Fin,(X_Lab,Y_Ini+10*DY),W_Lab,Alt,"Semiangolo del divergente, gradi");
      E_BetaD : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+10*DY),W_Edit,Alt,"4.0");
      L_VelVap : Label_Type := Label(Fin,(X_Lab,Y_Ini+11*DY),W_Lab,Alt,"Velocità del vapore, m/s");
      E_VelVap : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+11*DY),W_Edit,Alt,"60.0");
      L_Densit : Label_Type := Label(Fin,(X_Lab,Y_Ini+12*DY),W_Lab,Alt,"Densità del liquido, kg/mc");
      E_Densit : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+12*DY),W_Edit,Alt,"1000.0");
      L_Calspec : Label_Type := Label(Fin,(X_Lab,Y_Ini+13*DY),W_Lab,Alt,"Calore specifico del liquido, J/kgK");
      E_Calspec : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+13*DY),W_Edit,Alt,"4186.0");
      Dia_N : Label_Type := Label(Fin,(X_DN-110,10),W_Lab,Alt,"Scegliere la grandezza, DN:");
      Dia_Nom : Listbox_Type := Listbox(Fin,(X_DN,30),W_Edit,220);
      L_Risult : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+1*DY),W_Lab,Alt,"Risultati :      ",Right);
      Delta_T : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+2*DY),2*W_Lab,Alt,"");
      Tem_Fin : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+3*DY),2*W_Lab,Alt,"");
      Pre_Fin : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+4*DY),2*W_Lab,Alt,"");
      Consumo : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+5*DY),2*W_Lab,Alt,"");
      Velocit : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+6*DY),2*W_Lab,Alt,"");
      Spessor : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+7*DY),2*W_Lab,Alt,"");
      DNvapor : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+8*DY),2*W_Lab,Alt,"");
      Vevapor : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+9*DY),2*W_Lab,Alt,"");
      Fine : Button_Type := Button(Fin,(X_Lab,Y_Ini_Ris+2*DY),W_Edit,Alt,"Fine",'Q');
      Calc : Button_Type := Button(Fin,(X_Lab,Y_Ini_Ris+3*DY),2*W_Edit,Alt,"Calcola",'X');
      Ripr : Button_Type := Button(Fin,(X_Lab,Y_Ini_Ris+4*DY),3*W_Edit,Alt,
                                   "Ripristina i predefiniti",'R');
      Ris_Cal : Valori_Calcolati;
      Testo : String(1..10);
   begin
      Append_Line(Dia_Nom,"20");  Append_Line(Dia_Nom,"25"); Append_Line(Dia_Nom,"32");
      Append_Line(Dia_Nom,"40");  Append_Line(Dia_Nom,"50"); Append_Line(Dia_Nom,"65");
      Append_Line(Dia_Nom,"80");  Append_Line(Dia_Nom,"100"); Append_Line(Dia_Nom,"125"); 
      Append_Line(Dia_Nom,"150"); Append_Line(Dia_Nom,"200"); Append_Line(Dia_Nom,"250"); 
      Append_Line(Dia_Nom,"300"); Append_Line(Dia_Nom,"350"); Append_Line(Dia_Nom,"400"); 
      Append_Line(Dia_Nom,"450"); Append_Line(Dia_Nom,"500"); 
      Select_Line(Dia_Nom,7);
      while Valid(Fin) loop
         case Next_Command is
            when 'X' =>
               declare
               begin
                  N_Sez := Positive'Value(Get_Text(Sezioni));
                  Num_Fori := Mio_Float'Value(Get_Text(Fori));
                  Angolo := Mio_Float'Value(Get_Text(Angol));
                  Port_Acq := Mio_Float'Value(Get_Text(Prt_Acq))/3600.0;
                  Pres_Acq := 1.0E5*Mio_Float'Value(Get_Text(Prs_Acq));
                  Pres_Vap := 1.0E5*Mio_Float'Value(Get_Text(Prs_Vap));
                  BetaD := Mio_Float'Value(Get_Text(E_BetaD));
                  Vel_Vap := Mio_Float'Value(Get_Text(E_VelVap));
		  Dens := Mio_Float'Value(Get_Text(E_Densit));
		  Cal_Spec := Mio_Float'Value(Get_Text(E_Calspec));
                  declare
                     Tsat : Mio_Float := LS.Vapore.Tem_Sat(Pres_Vap);
                     Temperatura : Mio_Float;
                  begin
                     if Get_Text(Tem_Vap) = "Saturo" then
                        Temp_Vap := Tsat;
                     else
                        Temperatura := Mio_Float'Value(Get_Text(Tem_Vap)) + 273.15;
                        if Temperatura < Tsat then
                           Temp_Vap := Tsat;
                           Set_Text(Tem_Vap,"Saturo");
                        else
                           Temp_Vap := Temperatura;
                        end if;
                     end if;
                  end;
                  Temp_Acq := Mio_Float'Value(Get_Text(Tmp_Acq));
                  DN := Positive'Value(Get_Text(Dia_Nom));
                  Dia_Int := Diametri(DN).Interno;
                  Put(Testo,Dia_Int,1,0);
                  Set_Text(L_Gola, Testo);
                  Dia_Fori := Diametri(DN).Dei_Fori;
                  Put(Testo,Dia_Fori,1,0);
                  Set_Text(L_For, Testo);
                  Dia_Int := Mio_Float'Value(Get_Text(Diagola));
                  Dia_Fori := Mio_Float'Value(Get_Text(Diafori));
		  Dens_Vap := LS.Vapore.Dens_V(Pres_Vap,Temp_Vap);
                  Ris_Cal := Calcola(Pres_Vap);
               exception
                  when Pressione_Acqua_Negativa =>
                     Show_Message("Diminuire la portata d'acqua ",
                                  "Pressione dell'acqua negativa");
                  when Pressione_Vapore_Insufficiente =>
                     Show_Message("                                  Aumentarla",
                                  "Poca pressione del vapore");
                  when Margine_Sulla_Saturazione_Superato =>
                     Show_Message("                                  Diminuire la pressione del vapore",
                                  "Troppo riscaldamento risp. alla pressione");
               end;
               Put(Testo,Ris_Cal.Riscaldamento,1,0);
               Set_Text(Delta_T,"Riscaldamento : " & Testo & " °C più che all'ingresso");
               Put(Testo,Ris_Cal.Temp_Finale,1,0);
               Set_Text(Tem_Fin,"Temperatura finale : " & Testo & " °C ");
               Put(Testo,1.0E-5*Ris_Cal.Pres_Finale,2,0);
               Set_Text(Pre_Fin,"Pressione finale : " & Testo & " bara");
               Put(Testo,Port_Acq/Area_Sez,1,0);
               Set_Text(Velocit,"Velocità agli attacchi : "  & Testo & " m/s");
               Put(Testo,Pi*Dia_Int/Num_fori - Dia_Fori,1,0);
               Set_Text(Spessor,"Spessore di metallo fra i fori : " & Testo & " mm");
               Put(Testo,3600.0*Ris_Cal.Consumo,1,0);
               Set_Text(Consumo,"Consumo : "  & Testo & " kg/h di vapore");
               Q_Vap := Ris_Cal.Consumo/Dens_Vap;
               Put(Testo, DND(Q_Vap, Vel_Vap));
               Set_Text(DNvapor, "Attacco del vapore DN" & Testo(8..10));
               Vel_Vap := Q_Vap*4.0/(Pi*(0.001*Mio_Float(DND(Q_Vap, Vel_Vap)))**2);
               Put(Testo, Vel_Vap, 1, 0);
               Set_Text(Vevapor, "Velocità effettiva del vapore : " & Testo & " m/s"); 
            when 'R' =>
               DN := Positive'Value(Get_Text(Dia_Nom));
               Dia_Int := Diametri(DN).Interno;
               Put(Testo,Dia_Int,3,0);
               Set_Text(Diagola, Testo);
               Set_Text(L_Gola, Testo);
               Dia_Fori := Diametri(DN).Dei_Fori;
               Put(Testo,Dia_Fori,3,0);
               Set_Text(Diafori, Testo);
               Set_Text(L_For, Testo);
            when 'Q' =>
               exit;
            when others => null;
         end case;
      end loop;
   end Esegui;
end LS.Risc_Tubi;
with Ada.Text_IO; use Ada.Text_IO;
with LS.Risc_Tubi; use LS.Risc_Tubi;
procedure LS.RiscTubi is
begin
   Esegui;
end LS.RiscTubi;

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
with LS.Vapore; use LS.Vapore;
with LS.Solve1D; use LS.Solve1D;
with JEWL.Simple_Windows; use JEWL.Simple_Windows;
with LS.Curve_Termocompressori; use LS.Curve_Termocompressori;
with LS.Gpv2_Comuni; use LS.Gpv2_Comuni;
procedure Gpv2 is
   H_Win : constant Positive := 400; -- Altezza della finestra
   W_Win : constant Positive := 1000; -- Larghezza della finestra
   X_Lab : constant Positive := 10;  -- Ascissa dell'inizio delle etichette
   X_Edit : constant Positive := 350;--    ""          ""   dei campi di introduzione
   W_Lab : constant Positive := 300; -- Larghezza delle etichette
   W_Edit : constant Positive := 100; --   ""      dei campi
   Alt : constant Positive := 20;    -- Altezza dei campi
   Y_Ini : constant Positive := 10;      -- Ordinata iniziale delle etichette
   Y_Ini_Ris : constant Positive := 475; --    ""       ""    dei risultati
   DY : constant Positive := 25;         -- Spostamenti verso il basso
   Fin : Frame_Type := Frame(H_Win,W_Win,"PROGETTO DI UN GRUPPO PER VUOTO A DUE STADI",'Q');
   L_Pasp : Label_Type := Label(Fin,(X_Lab,Y_Ini),W_Lab,Alt,"Pressione di aspirazione, mbar");
   E_Pasp : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini),W_Edit,Alt,"200.0");
   L_Tasp: Label_Type := Label(Fin,(X_Lab,Y_Ini+DY),W_Lab,Alt,"Temperatura all'aspirazione, gradi C");
   E_Tasp : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+DY),W_Edit,Alt,"33.0");
   L_Pmot: Label_Type := Label(Fin,(X_Lab,Y_Ini+2*DY),W_Lab,Alt,"Pressione motrice, bar");
   E_Pmot : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+2*DY),W_Edit,Alt,"5.0");
   L_Tmot : Label_Type := Label(Fin,(X_Lab,Y_Ini+3*DY),W_Lab,Alt,"Temperatura del vapore motore, gradi C; saturo se = 0");
   E_Tmot : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+3*DY),W_Edit,Alt,"0.0");
   L_Tacq : Label_Type := Label(Fin,(X_Lab,Y_Ini+4*DY),W_Lab,Alt,"Temperatura dell'acqua di raffreddamento, gradi C");
   E_Tacq : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+4*DY),W_Edit,Alt,"30.0");
   L_Pfin : Label_Type := Label(Fin,(X_Lab,Y_Ini+5*DY),W_Lab,Alt,"Pressione finale, mbar");
   E_Pfin : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+5*DY),W_Edit,Alt,"1050.0");
   L_Mvap : Label_Type := Label(Fin,(X_Lab,Y_Ini+6*DY),W_Lab,Alt,"Portata di vapore trascinato, kg/h");
   E_Mvap : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+6*DY),W_Edit,Alt,"10.0");
   L_Masp : Label_Type := Label(Fin,(X_Lab,Y_Ini+7*DY),W_Lab,Alt,"Portata di incondensabili, kg/h");
   E_Masp : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+7*DY),W_Edit,Alt,"50.0");
   L_Pmol : Label_Type := Label(Fin,(X_Lab,Y_Ini+8*DY),W_Lab,Alt,"Peso molecolare degli incondensabili, kg/kmole");
   E_Pmol : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+8*DY),W_Edit,Alt,"29.0");
   L_Gamm : Label_Type := Label(Fin,(X_Lab,Y_Ini+9*DY),W_Lab,Alt,".. e loro costante adiabatica");
   E_Gamm : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+9*DY),W_Edit,Alt,"1.4");
   L_Sovr : Label_Type := Label(Fin,(X_Lab,Y_Ini+10*DY),W_Lab,Alt,"Sovrapposizione delle pressioni, %");
   E_Sovr : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+10*DY),W_Edit,Alt,"0.0");
   L_Kor1 : Label_Type := Label(Fin,(X_Lab,Y_Ini+11*DY),W_Lab,Alt,"Peggioramento del 1.o stadio rispetto alla Korting, %");
   E_Kor1 : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+11*DY),W_Edit,Alt,"10.0");
   L_Kor2 : Label_Type := Label(Fin,(X_Lab,Y_Ini+12*DY),W_Lab,Alt,"Peggioramento del 2.o stadio rispetto alla Korting, %");
   E_Kor2 : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+12*DY),W_Edit,Alt,"5.0");
   L_Pint : Label_Type := Label(Fin,(X_Lab,Y_Ini+13*DY),W_Lab,Alt,"Pressione intermedia, mbar; se = 0 ottimizza");
   E_Pint : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+13*DY),W_Edit,Alt,"0.0");
   L_Diff_Temp : Label_Type := Label(Fin,(X_Lab,Y_Ini+14*DY),W_Lab,Alt,"Subcooling; se 0, secondo Korting");
   E_Diff_Temp : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+14*DY),W_Edit,Alt,"0.0");
   L_Perd_Carico : Label_Type := Label(Fin,(X_Lab,Y_Ini+15*DY),W_Lab,Alt,"Perdita di carico nel condensatore, mbar");
   E_Perd_Carico : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+15*DY),W_Edit,Alt,"5.0");
   L_File : Label_Type := Label(Fin,(X_Lab,Y_Ini+16*DY),W_Lab,Alt,"Nome del file dei risultati, max. 60 caratteri");
   E_File : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini+16*DY),4*W_Edit,Alt,"gruppo_per_vuoto");
   L_Risul1 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris),W_Lab,Alt,"Consumo di vapore, kg/h      ",Right);
   L_Consum : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris),W_Lab,Alt,"");
   L_Risul2 : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris+DY),W_Lab,Alt,"  alla pressione intermedia di mbar ",Right);
   L_Press : Label_Type := Label(Fin,(X_Edit,Y_Ini_Ris+DY),W_Lab,Alt,"");
   Calc : Button_Type := Button(Fin,(X_Lab,Y_Ini_Ris+2*DY),2*W_Edit,Alt,"Calcola",'X');
   Fine : Button_Type := Button(Fin,(X_Lab,Y_Ini_Ris+3*DY),W_Edit,Alt,"Fine",'Q');
   Testo : String(1..10);
   Risultati : File_Type;
begin
-- Create(Risultati, Out_File, Get_Text(E_File));
-- Set_Output(Risultati);
   while Valid(Fin) loop
      case Next_Command is
         when 'X' =>
            P_Asp := 100.0 * Mio_Float'Value(Get_Text(E_Pasp));
            T_Asp := Mio_Float'Value(Get_Text(E_Tasp));
            P_Mot := 1.0E5 * Mio_Float'Value(Get_Text(E_Pmot));
            T_Mot := Mio_Float'Value(Get_Text(E_Tmot));
            if T_Mot <= 0.0 then T_Mot := Tem_Sat(P_Mot) - T0; end if;
            T_Acq := Mio_Float'Value(Get_Text(E_Tacq));
            P_Fin := 100.0*Mio_Float'Value(Get_Text(E_Pfin));
            M_Vap := Mio_Float'Value(Get_Text(E_Mvap));
            M_Asp := Mio_Float'Value(Get_Text(E_Masp));
            P_Mol := Mio_Float'Value(Get_Text(E_Pmol));
            Cost_Ad := Mio_Float'Value(Get_Text(E_Gamm));
            Sovr := Mio_Float'Value(Get_Text(E_Sovr));
            Kort1 := Mio_Float'Value(Get_Text(E_Kor1));
            Kort2 := Mio_Float'Value(Get_Text(E_Kor2));
            P_Int := 100.0*Mio_Float'Value(Get_Text(E_Pint));
            Diff_Temp := Mio_Float'Value(Get_Text(E_Diff_Temp));
	    Perd_Carico := 100.0*Mio_Float'Value(Get_Text(E_Perd_Carico));
            Cp := (Rgas/P_Mol)*Cost_Ad/(Cost_Ad - 1.0);
            begin
               declare
                  P_Interm : constant Mio_Float := Sqrt(P_Asp*P_Fin);
                  Fraz : constant Mio_Float := 0.5;
                  Num_Punti : constant Positive := 10000;
--                Delta_P : constant Mio_Float := Fraz*P_Interm/Mio_Float(Num_Punti);
                  Delta_P : constant Mio_Float := P_Interm/Mio_Float(Num_Punti);          
                  Pressioni, Consumi : Vettore(1..Num_Punti);
                  Consumo_Indice : Valore_Indice;
               begin
                  for I in 1..Num_Punti loop
                     Pressioni(I) := P_Interm*(1.0 - 0.5*Fraz) + Mio_Float(I)*Delta_P;
                     Consumi(I) := Calc_Port_Motr(Pressioni(I));
                  end loop;
                  Consumo_Indice := Min(Consumi);
                  Press_Cons_Min := Pressioni(Consumo_Indice.Indice);
               end;
               if P_Int > 0.0 then
                  Ottimizza := False;
                  Put(Testo, Calc_Port_Motr(P_Int), 3, 0);
                  Set_Text(L_Consum, Testo);
                  Put(Testo, 0.01*P_Int, 3, 0);
                  Set_Text(L_Press, Testo);
               else
                  Ottimizza := True;
                  Consumo_Minimo := Calc_Port_Motr(Press_Cons_Min);
                  Put(Testo, Consumo_Minimo, 3, 0);
                  Set_Text(L_Consum, Testo & " (minimo)");
                  Put(Testo, 0.01*Press_Cons_Min, 3, 0);
                  Set_Text(L_Press, Testo);
               end if;
	       begin
		  Open(Risultati, Append_File, Get_Text(E_File));
	       exception
		  when Name_Error => Create(Risultati, Append_File, Get_Text(E_File));
	       end;
               Set_Output(Risultati);
               Stampe;
               Close(Risultati);
            exception
               when others => null;
            end;
         when 'Q' => 
            exit;
         when others => null;
      end case;
   end loop;
end Gpv2;


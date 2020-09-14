with Ada.Numerics; use Ada.Numerics;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;
with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with LS.Integer_IO; use LS.Integer_IO;
with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;
with LS.Utili; use LS.Utili;
with LS.Vapore; use LS.Vapore;
with LS.Solve1D; use LS.Solve1D;
with LS.Curve_Termocompressori; use LS.Curve_Termocompressori;
with LS.Gpv2_Comuni; use LS.Gpv2_Comuni;
with Ada.Command_Line;

procedure Gpv2_Re is
   
   procedure Ricopia(Dati : String) is begin
      New_Line; Put_Line("Dati del calcolo:");
      P_Asp := Leggi_Dato(Dati, "p_asp", 50.0, " Pressione di aspirazione, mbar", 100.0);
      T_Asp := Leggi_Dato(Dati, "t_asp", 20.0, " Temperatura all'aspirazione, gradi C");
      P_Mot := Leggi_Dato(Dati, "p_mot", 5.0, " Pressione motrice, bar", 1.0E5);
      T_Mot := Leggi_Dato(Dati, "t_mot", Tem_Sat(P_Mot) - TK0, " Temperatura del vapore motore, gradi C; predefinita quella di saturazione");
      T_Acq := Leggi_Dato(Dati, "t_acq", 20.0, " Temperatura dell'acqua di raffreddamento, gradi C");
      P_Fin := Leggi_Dato(Dati, "p_fin", 1050.0, " Pressione di mandata, mbar", 100.0);
      M_Vap := Leggi_Dato(Dati, "m_vap", 10.0, " Portata di vapore trascinato, kg/h");
      M_Asp := Leggi_Dato(Dati, "m_asp", 10.0, " Portata di incondensabili, kg/h");
      P_Mol := Leggi_Dato(Dati, "p_mol", 29.0, " Peso molecolare degli incondensabili");
      Cost_Ad := Leggi_Dato(Dati, "cost_ad", 1.4, " Rapporto Cp/Cv degli incondensabili");
      Sovr := Leggi_Dato(Dati, "sovr", 0.0, " Sovrapposizione delle pressioni di mandata e aspirazione, % ");
      Kort1 := Leggi_Dato(Dati, "kort1", 10.0, " Peggioramento del 1.mo stadio rispetto alla Korting, % ");
      Kort2 := Leggi_Dato(Dati, "kort2", 5.0, " Peggioramento del 2.do stadio rispetto alla Korting, % ");
      P_Int := Leggi_Dato(Dati, "p_int", 0.0, " Pressione intermedia, mbar", 100.0);
      Diff_Temp := Leggi_Dato(Dati, "diff_temp", 0.0, " Subcooling (gradi C); se 0, secondo Korting");
      Perd_Carico := Leggi_Dato(Dati, "perd_carico", 5.0, " Perdita di carico nel condensatore, mbar", 100.0);
      Cp := (Rgas/P_Mol)*Cost_Ad/(Cost_Ad - 1.0);
   end Ricopia;
   
begin
   if Ada.Command_Line.Argument_Count = 1 then
      declare
         Dati : constant String := Ada.Command_Line.Argument(1);
      begin
         Ricopia(Dati);
      end;
   else
      New_Line;
      Put_Line(" Progetto di un gruppo per vuoto a due stadi");
      Put_Line(" -------------------------------------------");
      Put_Line(" Calcolo con i valori predefiniti");
      Put_Line(" --------------------------------");
      Put_Line(" Esempio di uso, cambiando solo M_Asp e P_Asp : ./gpv2_re "" m_asp 30.0 p_asp 70.0 "" ");
      New_Line;
      Ricopia(" ");
   end if;
   begin
      declare
         P_Interm : constant Mio_Float := Sqrt(P_Asp*P_Fin);
         Fraz : constant Mio_Float := 0.5;
         Num_Punti : constant Positive := 10000;
         Delta_P : constant Mio_Float := P_Interm/Mio_Float(Num_Punti);
         Pressioni, Consumi : Vettore(1..Num_Punti);
         Consumo_Indice : Valore_Indice;
      begin
         for I in 1..Num_Punti loop
            Pressioni(I) := P_Interm*(1.0 - 0.5*Fraz) + Mio_Float(I)*Delta_P;
            Consumi(I) := Calc_Port_Motr(Pressioni(I));
         end loop;
         Stampa_Curve(Pressioni, Consumi, "consumo_vs_press_intermedia");
         Consumo_Indice := Min(Consumi);
         Press_Cons_Min := Pressioni(Consumo_Indice.Indice);
      end;
      if P_Int > 0.0 then
         Ottimizza := False;
         New_Line; Put("Consumo di vapore = ");
         Put(Calc_Port_Motr(P_Int), 3, 2, 0);
         Put(" kg/h"); New_Line;
      else
         Ottimizza := True;
         Consumo_Minimo := Calc_Port_Motr(Press_Cons_Min); 
         New_Line; Put("Minimo consumo di vapore = ");
         Put(Consumo_Minimo, 3, 2, 0);
         Put(" kg/h (minimo)"); New_Line;
         Put("  alla pressione intermedia di ");
         Put(Press_Cons_Min*0.01, 3, 2, 0);
         Put(" mbar"); New_Line;
      end if;    
      Stampe;
   exception
      when others => null;
   end;
end Gpv2_Re;


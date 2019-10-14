with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics; use Ada.Numerics;
with LS.Float_IO; use LS.Float_IO;
with LS.Numerico; use LS.Numerico;
with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;
with LS.Utili; use LS.Utili;
generic
   Num_Curve : Positive;
   Num_Punti : Positive;
package LS.Interpolazione_Curve is
   type Curve is new Matrice(1..Num_Punti,1..2);
   type Curva_E_Valore is record
      Valore : Mio_Float;
      Curva : Curve;
   end record;
   type Dati_Curve is array (1..Num_Curve) of Curva_E_Valore;
   procedure Interpola(Matr : in Curve; X : in Mio_Float; Interpolaz : out Mio_Float; Estrap : out Boolean);
   -- Interpola una singola curva; eventualmente estrapola
   function Ascisse_Ordinate(Matr : in Curve) return Boolean; -- controlla che le ascisse siano in ordine, crescente o decrescente
   procedure Interpola_Curve(Dati : in Dati_Curve; Ascissa, Valore_Curva : in Mio_Float; Risult : out Mio_Float; 
											 Estrap : out Boolean);
   -- Interpola fra due curve; eventualmente estrapola
--   function Valori_Ordinati(Dati : in Dati_Curve) return Boolean; -- controlla che i valori relativi alle varie curve siano in ordine, crescente o decrescente
   procedure Disegna_Curve(Dati : in Dati_Curve);
end LS.Interpolazione_Curve;

package body LS.Interpolazione_Curve is
   procedure Interpola(Matr : in Curve; X : in Mio_Float; Interpolaz : out Mio_Float; Estrap : out Boolean) is
      -- Interpola una singola curva; eventualmente estrapola
      type Paio is record
         I,J : Positive;
      end record;
      Lung : Positive := 2; -- Ci vogliono almeno due termini per interpolare
      Punti : Paio; -- := (1, Lung);
      I, J : Positive;
      Valori_Crescenti : Boolean := Matr(1,1) < Matr(2,1);
      Esterno_Sinistro, Esterno_Destro : Boolean;
      function Bisez(Cp : Paio) return Paio is
         Ris : Paio := Cp;
         K : Positive;
      begin
         if Cp.J - Cp.I > 1 then
            K := (Cp.I + Cp.J)/2;
            if (Valori_Crescenti and Matr(Cp.I, 1) <= X and X < Matr(K, 1)) or
               (not Valori_Crescenti and Matr(K, 1) <= X and X < Matr(Cp.I, 1)) then
               Ris := (Cp.I, K);
            else
               Ris := (K, Cp.J);
            end if;
            Ris := Bisez(Ris);
         end if;
         return Ris;
      end Bisez;
   begin
      -- Consideriamo lo zero, o gli zeri finali
      for I in 3..Matr'Length(1) loop
         exit when Matr(I, 1) = 0.0 and
           (Matr(I-1, 1) = 0.0 or (Matr(I-1, 1) < 0.0 and Matr(I-2, 1) > Matr(I-1, 1))
                          or (Matr(I-1, 1) > 0.0 and Matr(I-2, 1) < Matr(I-1, 1)));
         Lung := Lung + 1;
      end loop;
      Esterno_Sinistro := (Valori_Crescenti and X < Matr(1,1)) or (not Valori_Crescenti and X < Matr(Lung,1));
      Esterno_Destro := (Valori_Crescenti and X > Matr(Lung,1)) or (not Valori_Crescenti and X > Matr(1,1));
      Estrap := False;
      if Esterno_Sinistro or Esterno_Destro then
         Estrap := True;
         if Esterno_Destro then
            I := Lung - 1;
            J := Lung;
         else
            I := 1;
            J := 2;
         end if;
      else
         Punti := (1, Lung);
         Punti := Bisez(Punti);
         I := Punti.I;
         J := Punti.J;
      end if;
      Interpolaz := Interpola(Matr(I,1), Matr(I,2), Matr(J,1), Matr(J,2), X); -- Matr(I,2) + (X - Matr(I,1))*(Matr(J,2) - Matr(I,2))/(Matr(J,1) - Matr(I,1)); 
   end Interpola;
   
   function Ascisse_Ordinate(Matr : in Curve) return Boolean is
      Risultato : Boolean := True;
      Valori_Crescenti : Boolean := Matr(1,1) < Matr(2,1);
      Confronto :  Boolean;
   begin
      for I in 1..Matr'Length(1)-1 loop
         Confronto := (if Valori_Crescenti then Matr(I,1) < Matr(I+1,1) else Matr(I,1) > Matr(I+1,1));
         Risultato := Risultato and Confronto;
         exit when Risultato = False;
       end loop;
       return Risultato;
   end Ascisse_Ordinate;
   
   procedure Interpola_Curve(Dati : in Dati_Curve; Ascissa, Valore_Curva : in Mio_Float; Risult : out Mio_Float; 
                             Estrap : out Boolean) is
      -- interpola fra due curve; eventualmente estrapola
      Prima_Curva, Seconda_Curva : Curve;
      Primo_Valore, Secondo_Valore : Mio_Float;
      Num_Curve : Positive := Dati'Length;
      Valori_Crescenti : Boolean := Dati(1).Valore < Dati(2).Valore;
      Esterno_Sinistro : Boolean := (Valori_Crescenti and Valore_Curva < Dati(1).Valore) 
	or (not Valori_Crescenti and Valore_Curva >= Dati(1).Valore);
      Esterno_Destro : Boolean := (Valori_Crescenti and Valore_Curva > Dati(Num_Curve).Valore) 
        or (not Valori_Crescenti and Valore_Curva <= Dati(Num_Curve).Valore);
      Ind_1, Ind_2 : Positive;
      Interp_1, Interp_2 : Mio_Float;
      Estrap_1, Estrap_2, Estrap_3 : Boolean := False;
   begin
      if Esterno_Destro or Esterno_Sinistro then
         Estrap_1 := True;
         if Esterno_Destro then
            Ind_1 := Num_Curve - 1;
            Ind_2 := Num_Curve;
         else
            Ind_1 := 1;
            Ind_2 := 2;
         end if;
         Prima_Curva := Dati(Ind_1).Curva;
         Seconda_Curva := Dati(Ind_2).Curva;
         Primo_Valore := Dati(Ind_1).Valore;
         Secondo_Valore := Dati(Ind_2).Valore;
      else
         for I in 2..Num_Curve loop
            Primo_Valore := Dati(I - 1).Valore;
            Secondo_Valore := Dati(I).Valore;
            if (Primo_Valore >= Valore_Curva and Valore_Curva > Secondo_Valore) or
               (Primo_Valore <= Valore_Curva and Valore_Curva < Secondo_Valore) 
            then 
               Prima_Curva := Dati(I - 1).Curva;
               Seconda_Curva := Dati(I).Curva;
               exit;
            end if;
         end loop;
      end if;
      Interpola(Prima_Curva, Ascissa, Interp_1, Estrap_2);
      Interpola(Seconda_Curva, Ascissa, Interp_2, Estrap_3);
--    if Esterno_Sinistro then Put("Esterno sinistro"); New_Line; end if;
--    if Esterno_destro then Put("Esterno destro"); New_Line; end if;
--    for I in 1..Prima_Curva'Length(1) loop
--       Put(Prima_Curva(I,1), 3, 2, 0); Put(Prima_Curva(I,2), 3, 2, 0); Put("; ");
--    end loop;
--    New_Line;
--    for I in 1..Seconda_Curva'Length(1) loop
--       Put(Seconda_Curva(I,1), 3, 2, 0); Put(Seconda_Curva(I,2), 3, 2, 0); Put("; ");
--    end loop;
--    New_Line;
--    Put("V1="&Primo_Valore'Img&"; I1="&Interp_1'Img&"; V2="&Secondo_Valore'Img&"; I2="&Interp_2'Img&"; Asc="&Ascissa'Img&"; Val="&Valore_Curva'Img); New_Line;
      Risult := Interpola(Primo_Valore, Interp_1, Secondo_Valore, Interp_2, Valore_Curva);
      Estrap := Estrap_1 or Estrap_2 or Estrap_3;
   end Interpola_Curve;
   
   procedure Disegna_Curve(Dati : in Dati_Curve) is
      Nuovo_file : File_Type;
   begin
      Create(Nuovo_File,Out_File,"prova_curve_log.plt");
      Set_Output(Nuovo_File);
      Put("set term postscript color"); New_Line;
      Put("set output '"); Put("prova_curve_log.ps"); New_Line;
      Put("plot'-' title '"); Put("prova_curve.plt"); Put("' with lines"); New_Line;
      for I in 1..Num_Curve Loop
         exit when Dati(I).Valore = 0.0;
         for J in 1..Num_Punti loop
            exit when Dati(I).Curva(J,1) = 0.0;
            Put(Log(Dati(I).Curva(J,1)));
            Put(Log(Dati(I).Curva(J,2)));
            New_Line;
            end loop;
         New_Line;
      end loop;
      Close(Nuovo_File);
      Create(Nuovo_File,Out_File,"prova_curve.plt");
      Set_Output(Nuovo_File);
      Put("set term postscript color"); New_Line;
      Put("set output '"); Put("prova_curve.ps"); New_Line;
      Put("plot'-' title '"); Put("prova_curve.plt"); Put("' with lines"); New_Line;
      for I in 1..Num_Curve Loop
         exit when Dati(I).Valore = 0.0;
         for J in 1..Num_Punti loop
            exit when Dati(I).Curva(J,1) = 0.0;
            Put(Dati(I).Curva(J,1));
            Put(Dati(I).Curva(J,2));
            New_Line;
            end loop;
         New_Line;
      end loop;
      Close(Nuovo_File);
      Set_Output(Standard_Output);
   end Disegna_Curve;

end LS.Interpolazione_Curve;

-- Generare automaticamente le procedure di IO di un programma
With Ada.Numerics; use Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with LS.Integer_IO; use LS.Integer_IO;
with LS.Utili; use LS.Utili;
generic -- questa parte serve per eseguire il programma che è stato preparato
   type Dati_Input is (<>);
   type Opzioni_Calcolo is (<>);
   type Risultati_Output is (<>);
package LS.LC_Gen is
   Solo_Informazione : exception;
   type Se_File is (File);
   Il_File : File_Type;
   package Dati_IO is new Enumeration_IO(Enum => Dati_Input); use Dati_IO;
   type Dato_Input is record
      Valr, Molt : Mio_Float; -- il valore predefinito e il moltiplicatore
      Unit, Desc : Unbounded_String; -- l'unità di misura e la descrizione della grandezza
   end record;
   Dati : array(Dati_Input) of Dato_Input;
   package Opzioni_IO is new Enumeration_IO(Enum => Opzioni_Calcolo); use Opzioni_IO;
   type Opzione is record
      Valida : Boolean := False;
      Desc : Unbounded_String;
   end record;
   Opzioni : array(Opzioni_Calcolo) of Opzione;
   package Risultati_IO is new Enumeration_IO(Enum => Risultati_Output); use Risultati_IO;
   type Risultato is record
      Valr, Molt : Mio_Float;
      Unit, Desc : Unbounded_String;
   end record;
   Risultati : array(Risultati_Output) of Risultato;
   Usa_File : Boolean := False;
   Filename : Unbounded_String;
   Dato : Dato_Input;
   Opz : Opzione;
   Ris : Risultato;
   procedure Lettura_Dati_Opzioni;
   procedure Lettura_Da_File(Filename : Unbounded_String);
   procedure Scrittura_Dati_Opzioni_Risultati;
   procedure Scrittura_Dati_Opzioni;
   procedure Scrittura_Risultati;
   procedure Scrittura;
   function T_U_S(Nome : String) return Unbounded_String renames To_Unbounded_String;
end LS.LC_Gen;
package body LS.LC_Gen is
   procedure Lettura_Dati_Opzioni is
   begin
      if Argument_Count = 0 then -- scrive su terminale l'elenco dei dati, opzioni, risultati 
         New_Line;
         Put("Lista dei dati di ingresso:"); New_Line;
         Put("-------------------------- "); New_Line;
         for Il_Dato in Dati_Input loop
            Dato := Dati(Il_Dato);
            Put (Il_Dato); 
            Put (" : " & Dato.Desc & " [" & Dato.Unit & "]" ); 
            Put ("; valore predefinito : "); Put (Dato.Valr, 3, 3, 0);
            New_Line;
         end loop;
         New_Line;
         Put("Lista delle opzioni:"); New_Line;
         Put("-------------------"); New_Line;
         for Una_Opzione in Opzioni_Calcolo loop
            Opz := Opzioni(Una_Opzione);
            Put (Una_Opzione); 
            Put (" : " & Opz.Desc); 
            New_Line;
         end loop;
         New_Line;
         Put("Lista dei risultati:"); New_Line;
         Put("------------------- "); New_Line;
         for Il_Risultato in Risultati_Output loop
            Ris := Risultati(Il_Risultato);
            Put (Il_Risultato); 
            Put (" : " & Ris.Desc & " [" & Ris.Unit & "]" ); 
            New_Line;
         end loop;
         New_Line;
         Put("Inoltre, "); New_Line;
         Put("FILE : "); Put("se si usa un file; seguito dal nome di questo"); New_Line;
         Put("Minuscole o maiuscole fa lo stesso"); New_Line;
         Raise Solo_Informazione;
      else
         -- lettura dei dati da un file, se richiesto, e poi dalla linea di comando
         for Arg in 1..Argument_Count loop
            begin
               if Se_File'Value(Argument(Arg)) = File then
                  Usa_File := True;
                  Filename := To_Unbounded_String(Argument(Arg + 1));
                  exit;
               end if;
            exception
               when others => null;
            end;
         end loop;
         if Usa_File and Esiste_Il_File(To_String(Filename)) then
            Lettura_Da_File(Filename);
--          Open(Il_File, In_File, To_String(Filename));
--          for I in 1..2 loop
--             Skip_Line(File => Il_File);
--          end loop;
--          for Un_Dato in Dati_Input loop
--             begin
--                Get(File => Il_File, Item => Dati(Un_Dato).Valr);
--                Skip_Line(File => Il_File);
--             exception
--                when others => null;
--                   begin
--                      Put("Errore leggendo i dati dal file"); 
--                      Put( Dati(Un_Dato).Valr );
--                      New_Line;
--                   end;
--             end;
--          end loop;
--          Close(Il_File);
         end if;
         for Un_Dato in Dati_Input loop
            for Arg in 1..Argument_Count - 1 loop
               begin
                  if Un_Dato = Dati_Input'Value(Argument(Arg)) then
                     Dati(Un_Dato).Valr := Mio_Float'Value(Argument(Arg + 1)); -- si riassegnano i predefiniti
                     exit;
                  end if;
               exception
                  when others => null;
               end;
            end loop;
         end loop;
         for Una_Opzione in Opzioni_Calcolo loop
            for Arg in 1..Argument_Count loop
               begin
                  if Una_Opzione = Opzioni_Calcolo'Value(Argument(Arg)) then
                     Opzioni(Una_Opzione).Valida := True;
                     exit;
                  end if;
               exception
                  when others => null;
               end;
            end loop;
         end loop;
      end if;
   end Lettura_Dati_Opzioni;
   procedure Lettura_Da_File(Filename : Unbounded_String) is
   begin
      Open(Il_File, In_File, To_String(Filename));
      for I in 1..2 loop
         Skip_Line(File => Il_File);
      end loop;
      for Un_Dato in Dati_Input loop
         begin
            Get(File => Il_File, Item => Dati(Un_Dato).Valr);
            Skip_Line(File => Il_File);
         exception
            when others => null;
--             begin
--                Put("Errore leggendo i dati dal file"); 
--                Put( Dati(Un_Dato).Valr );
--                New_Line;
--             end;
         end;
      end loop;
      Close(Il_File);
   end Lettura_Da_File;
   procedure Scrittura_Dati_Opzioni_Risultati is
   begin
      Scrittura_Dati_Opzioni;
      Scrittura_Risultati;
   end Scrittura_Dati_Opzioni_Risultati;
   procedure Scrittura_Dati_Opzioni is
   begin
      Put("Dati del calcolo:"); New_Line;
      Put("---------------- "); New_Line;
      for Il_Dato in Dati_Input loop
         Dato := Dati(Il_Dato);
         Put (Il_Dato); 
         Put (" : ");
         Put (Dato.Valr, 3, 3, 0);
         Put (", " & Dato.Desc & " [" & Dato.Unit & "]" ); 
         New_Line;
      end loop;
      New_Line;
      Put("Opzioni usate:"); New_Line;
      Put("-------------"); New_Line;
      for Una_Opzione in Opzioni_Calcolo loop
         Opz := Opzioni(Una_Opzione);
         if Opz.Valida then
            Put (Una_Opzione); 
            Put (" : " & Opz.Desc); 
            New_Line;
         end if;
      end loop;
      New_Line;
   end Scrittura_Dati_Opzioni;
   procedure Scrittura_Risultati is
   begin
      Put("Risultati del calcolo:"); New_Line;
      Put("--------------------- "); New_Line;
      for Il_Risultato in Risultati_Output loop
         Ris := Risultati(Il_Risultato);
         Put (Il_Risultato); 
         Put (" = ");
         Put( Ris.Valr, 3, 3, 0);
         Put (" : " & Ris.Desc & " [" & Ris.Unit & "]" ); 
         New_Line;
      end loop;
      New_Line;
   end Scrittura_Risultati;
   procedure Scrittura is
   begin
      Scrittura_Dati_Opzioni_Risultati;
      if Usa_File then
         if not Esiste_Il_File(To_String(Filename)) then
            Create(Il_File, Out_File, To_String(Filename));
            Close(Il_File);
         end if;
--       Open(Il_File, Out_File, To_String(Filename)&".ris");
         Open(Il_File, Out_File, To_String(Filename));
         Set_Output(Il_File);
         Scrittura_Dati_Opzioni_Risultati;
         Close(Il_File);
      end if;
   end Scrittura;
end LS.LC_Gen;
with LS.LC_Gen; generic package LS.Linea_Comando renames LS.LC_Gen;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with LS.Utili; use LS.Utili;
with LS.LC_Gen;
procedure LS.Prepara is
   type Dati_Inp is (Aaa); -- muti
   type Opz_Cal is (Bbb); 
   type Ris_Out is (Ccc); 
   package LC is new 
     LS.LC_Gen(Dati_Input => Dati_Inp, Opzioni_Calcolo => Opz_Cal, Risultati_Output => Ris_Out);
   use LC;
--   Lung : Positive;
--   Linea : Unbounded_String;
   Num_Righe : Positive := 100;
   Niente_File, Errore_Nei_Dati : exception;
   File_Lett, File_Ada, File_Corpo, File_Sh, File_Jewl, File_Jewl_Sh : File_Type;
   function Separatrice(Linea : String) return Boolean is
      Ris : Boolean := False;
      Lettera : Character;
   begin -- una linea separatrice inizia con un ; eventualmente preceduto da spazi
      for Ind in Linea'Range loop
         Lettera := Linea(Ind);
         exit when Lettera /= ' ';
      end loop;
      return Lettera = ';';
   end Separatrice;
   procedure Spazi_Bianchi(Num : Positive) is
   begin
      for I in 1..Num loop
         Put(' ');
      end loop;
   end Spazi_Bianchi;
   type Dati is
      record
         Nome, Desc, Unit, Molt, Valr : Unbounded_String;
      end record;
   Dato : Dati; 
   I_Dati : array(1..Num_Righe) of Dati := (others => Dato);
   type Opzioni is
      record
         Nome, Desc : Unbounded_String;
      end record;
   Opzione : Opzioni;
   Le_Opzioni : array(1..Num_Righe) of Opzioni := (others => Opzione);
   type Risultati is
      record
         Nome, Desc, Unit, Molt, Valr : Unbounded_String;
      end record;
   Risl : Risultati;
   I_Risultati : array(1..Num_Righe) of Risultati := (others => Risl);
   Numero_Dati, Numero_Opzioni, Numero_Risultati, PV_1, PV_2, PV_3, PV_4 : Natural := 0;
   Suffx : String := "_0"; -- suffisso distinguente le variabili dai nomi
   procedure Scrivi_Corpo(Iniziale, Inmiofloat, Inboolean, Finale, Suffisso : String) is
   begin
      Put(Iniziale &" Corpo(");
      for Cont in 1..Numero_Dati - 1 loop
         Put(I_Dati(Cont).Nome & Suffisso & "," );
      end loop;
      Put(I_Dati(Numero_Dati).Nome & Suffisso & Inmiofloat);
      if Numero_Opzioni > 0 then
         for Cont in 1..Numero_Opzioni - 1 loop
            Put(Le_Opzioni(Cont).Nome & Suffisso & "," );
         end loop;
         Put(Le_Opzioni(Numero_Opzioni).Nome & Suffisso & Inboolean);
      end if;
      for Cont in 1..Numero_Risultati - 1 loop
         Put(I_Risultati(Cont).Nome & Suffisso & ",");
      end loop;
      Put(I_Risultati(Numero_Risultati).Nome & Suffisso & Finale);
      New_Line;
   end Scrivi_Corpo;
begin
   if Argument_Count = 0 then
      New_Line;
      Put ("Richiede il nome di un file così formato: inizia con le righe dei dati,");
      New_Line; New_Line;
      Put("Nome_del_dato; Sua_descrizione; Unità_Di_Misura; Moltiplicatore; Valore_predefinito"); New_Line;
      Put("Esempio: Press_Mot; Pressione motrice; bar ; 1.0E5 ; 5.0 "); New_Line;
      Put("Dopo ogni punto e virgola deve esserci almeno uno spazio"); New_Line;
      Put(" ... "); New_Line;
      Put("Si possono omettere i campi, dalla descrizione in poi, omettendo anche i ; di separazione; maiuscole e minuscole fanno lo stesso"); New_Line;
      Put("... poi si mette una riga con solo un ; (punto e virgola), "); New_Line;
      Put(" ; "); New_Line;
      Put("e si comincia ad elencare le opzioni: "); New_Line;
      Put("Nome_dell_opzione; Sua_descrizione"); New_Line;
      Put("Esempio: kPa; Pressione in kPa"); New_Line;
      Put(" ... "); New_Line;
      Put("poi ancora una riga col punto e virgola, e segue la lista dei risultati"); New_Line;
      Put("Nome_del_risultato; Sua_descrizione; Unità_Di_Misura; Moltiplicatore"); New_Line;
      Put("Esempio: port; Portata aspirata; kg/h ; 3600.0"); New_Line;
      Put(" ... "); New_Line; New_Line;
      Put("I tre gruppi (dati, opzioni, risultati) devono comparire in questo ordine reciproco"); New_Line;
      Put("I numeri vanno dati come Float Ada, ossia col punto decimale seguito da almeno una cifra"); New_Line;
      Put("Il file deve avere non più di "); Put(Num_Righe + 3, 3); Put(" righe"); New_Line;
      Put("Se non ci sono opzioni, ci saranno due ; in due righe successive"); New_Line;
      New_Line;
      raise Solo_Informazione;
   else
      Filename := To_Unbounded_String(Argument(1));
      if not Esiste_Il_File(To_String(Filename)) then
         raise Niente_File;
      else
         Open(File_Lett, In_File, To_String(Filename));
         Set_Input(File_Lett);
      end if;
   end if;
   loop
      declare
         Linea : String := Get_Line;
         Lung : Positive := Linea'Length;
      begin
         Put(Linea); New_Line;   
         exit when Separatrice(Linea) or End_Of_File(File_Lett);
         Numero_Dati := Numero_Dati + 1;
         I_Dati(Numero_Dati).Molt := T_U_S("1.0");
         I_Dati(Numero_Dati).Valr := T_U_S("1.0");
         Pv_1 := Index(Linea, ";");
         if Pv_1 = 0 then -- se si è scritto solo il nome del dato..
            I_Dati(Numero_Dati).Nome := T_U_S(Linea(1..Lung));
         else
            I_Dati(Numero_Dati).Nome := T_U_S(Linea(1..Pv_1 - 1));              
            Pv_2 := Index(Linea(Pv_1 + 1..Lung), ";");
            if Pv_2 = 0 then
               I_Dati(Numero_Dati).Desc := T_U_S(Linea(Pv_1 + 1..Lung));
            else
               I_Dati(Numero_Dati).Desc := T_U_S(Linea(Pv_1 + 1..Pv_2 - 1));
               Pv_3 := Index(Linea(Pv_2 + 1..Lung), ";");
               if PV_3 = 0 then
                  I_Dati(Numero_Dati).Unit := T_U_S(Linea(Pv_2 + 1..Lung));
               else
                  I_Dati(Numero_Dati).Unit := T_U_S(Linea(Pv_2 + 1..Pv_3 - 1));
                  Pv_4 := Index(Linea(Pv_3 + 1..Lung), ";");
                  if Pv_4 = 0 then
                     I_Dati(Numero_Dati).Molt := T_U_S(Linea(Pv_3 + 1..Lung));
                  else
                     I_Dati(Numero_Dati).Molt := T_U_S(Linea(Pv_3 + 1..PV_4 - 1));
                     I_Dati(Numero_Dati).Valr := T_U_S(Linea(Pv_4 + 1..Lung));
                  end if;
               end if;
            end if;
         end if;
      exception
         when others => New_Line; Put(Linea); New_Line;
      end;
   end loop;
   loop
      declare
         Linea : String := Get_Line;
         Lung : Positive := Linea'Length;
      begin
         Put(Linea); New_Line;
         exit when Separatrice(Linea) or End_Of_File(File_Lett);
         Numero_Opzioni := Numero_Opzioni + 1;
         PV_1 := Index(String(Linea), ";");
         if PV_1 = 0 then
            Le_Opzioni(Numero_Opzioni).Nome := T_U_S(Linea(1..Lung));
         else
            Le_Opzioni(Numero_Opzioni).Nome := T_U_S(Linea(1..PV_1 - 1));
            Le_Opzioni(Numero_Opzioni).Desc := T_U_S(Linea(PV_1 + 1..Lung));
         end if;
      exception
         when others => New_Line; Put(Linea); New_Line;
      end;
   end loop;
   loop
      declare
         Linea : String := Get_Line;
         Lung : Positive := Linea'Length;
      begin
         Put(Linea); New_Line;
         Numero_Risultati := Numero_Risultati + 1;
         I_Risultati(Numero_Risultati).Molt := T_U_S("1.0");
         I_Risultati(Numero_Risultati).Valr := T_U_S("0.0");
         Pv_1 := Index(String(Linea), ";");
         if Pv_1 = 0 then
            I_Risultati(Numero_Risultati).Nome := T_U_S(Linea(1..Lung));
         else
            I_Risultati(Numero_Risultati).Nome := T_U_S(Linea(1..Pv_1 - 1));              
            Pv_2 := Index(Linea(Pv_1 + 1..Lung), ";");
            if Pv_2 = 0 then
               I_Risultati(Numero_Risultati).Desc := T_U_S(Linea(Pv_1 + 1..Lung));
            else
               I_Risultati(Numero_Risultati).Desc := T_U_S(Linea(Pv_1 + 1..Pv_2 - 1));
               Pv_3 := Index(Linea(Pv_2 + 1..Lung), ";");
               if PV_3 = 0 then
                  I_Risultati(Numero_Risultati).Unit := T_U_S(Linea(Pv_2 + 1..Lung));
               else
                  I_Risultati(Numero_Risultati).Unit := T_U_S(Linea(Pv_2 + 1..Pv_3 - 1));
                  I_Risultati(Numero_Risultati).Molt := T_U_S(Linea(Pv_3 + 1..Lung));
               end if;
            end if;
         end if;
         exit when End_Of_File(File_Lett); 
      exception
         when others => New_Line; Put(Linea); New_Line;
      end;
   end loop;
   Close(File_Lett);
   -- Scrive l'abbozzo del programma Ada a linea di comando -----------------------------------------------
   Create(File_Ada, Out_File, To_String(Filename) & ".adx");
   Set_Output(File_Ada);
   Put("with Ada.Numerics; use Ada.Numerics;"); New_Line;
   Put("with Ada.Strings.Fixed; use Ada.Strings.Fixed;"); New_Line;
   Put("with Ada.Text_IO; use Ada.Text_IO;"); New_Line;
   Put("with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;"); New_Line;
   Put("with Ada.Calendar; use Ada.Calendar;"); New_Line;
   Put("with LS.Numerico; use LS.Numerico;"); New_Line;
   Put("with LS.Float_IO; use LS.Float_IO;"); New_Line;
   Put("with LS.Integer_IO; use LS.Integer_IO;"); New_Line;
   Put("with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;"); New_Line;
   Put("with LS.Utili; use LS.Utili;"); New_Line;
   Put("with LS.Solve1D; use LS.Solve1D;"); New_Line;
   Put("with LS.Solve1D_Gen;"); New_Line;
   Put("with LS.Vapore; use LS.Vapore;"); New_Line;
   Put("with LS.Flusso_Isentropico; use LS.Flusso_Isentropico;"); New_Line;
   Put("with LS.Pdc_Diff;"); New_Line;
   Put("with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;"); New_Line;
   Put("with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;"); New_Line;
   Put("with Ada.Exceptions;"); New_Line;
   Put("with LS.LC_Gen;"); New_Line;
   Put("package LS." & Filename & " is"); New_Line;
   Spazi_Bianchi(3);
   for Cont in 1..Numero_Dati - 1 loop
      Put(I_Dati(Cont).Nome & Suffx & ", ");
   end loop;
   Put(I_Dati(Numero_Dati).Nome & Suffx & " : Mio_Float;"); 
   New_Line;
   Spazi_Bianchi(3);
   for Cont in 1..Numero_Risultati - 1 loop
      Put(I_Risultati(Cont).Nome & Suffx & ", ");
   end loop;
   Put(I_Risultati(Numero_Risultati).Nome & Suffx & " : Mio_Float;"); 
   New_Line;
   if Numero_Opzioni > 0 then
      Spazi_Bianchi(3);
      for Cont in 1..Numero_Opzioni - 1 loop
         Put(Le_Opzioni(Cont).Nome & Suffx & ", ");
      end loop;
      Put(Le_Opzioni(Numero_Opzioni).Nome & Suffx & " : Boolean := False;"); 
      New_Line;
   end if;
   Put("   type Dati_Inp is (");
   for Cont in 1..Numero_Dati - 1 loop
      Put(I_Dati(Cont).Nome & ", ");
   end loop;
   Put(I_Dati(Numero_Dati).Nome & ");"); 
   New_Line;
   Put("   type Opz_Cal is (");
   if Numero_Opzioni > 0 then
      for Cont in 1..Numero_Opzioni - 1 loop
         Put(Le_Opzioni(Cont).Nome & ", ");
      end loop;
      Put(Le_Opzioni(Numero_Opzioni).Nome & ");"); 
      New_Line;
   else
      Put(" Dumm );"); New_Line;
   end if;
   Put("   type Ris_Out is (");
   for Cont in 1..Numero_Risultati - 1 loop
      Put(I_Risultati(Cont).Nome & ", ");
   end loop;
   Put(I_Risultati(Numero_Risultati).Nome & ");"); 
   New_Line;
   Put("   package LC is new"); New_Line;
   Put("      LS.LC_Gen(Dati_Input => Dati_Inp, Opzioni_Calcolo => Opz_Cal, Risultati_Output => Ris_Out);"); New_Line;
   Put("   use LC;"); New_Line;
   Put("   procedure " & Filename & "_0 ;"); New_Line;
   Put("end LS." & Filename & ";"); New_Line;
   Put("package body LS." & Filename & " is"); New_Line;
   Scrivi_Corpo(Iniziale => "   procedure", Inmiofloat => " : in Mio_Float;", Inboolean => " : in Boolean;", 
		Finale => " : out Mio_Float) is separate;", Suffisso => " ");
   Put("   procedure " & Filename & "_0 is"); New_Line;
   Put("   begin"); New_Line;
   Put("      Dati := ("); New_Line;
   for Cont in 1..Numero_Dati - 1 loop
      Dato := I_Dati(Cont);
      Spazi_Bianchi(9);
      Put(Dato.Nome &" => (Valr => " & Dato.Valr & ", Molt => " & Dato.Molt & ", Unit => T_U_S("""& Dato.Unit & """), Desc => T_U_S("""
	    & Dato.Desc & """)),");
      New_Line;
   end loop;
   Dato := I_Dati(Numero_Dati);
   Spazi_Bianchi(9);
   Put(Dato.Nome & " => (Valr => " & Dato.Valr & ", Molt => " & Dato.Molt & ", Unit => T_U_S("""& Dato.Unit & """), Desc => T_U_S("""
	 & Dato.Desc & """)) );");
   New_Line;
   if Numero_Opzioni > 0 then
      Put("      Opzioni := ("); New_Line;
      for Cont in 1..Numero_Opzioni - 1 loop
         Opzione := Le_Opzioni(Cont);
         Spazi_Bianchi(6);
         Put(Opzione.Nome & " => (Desc => T_U_S("""& Opzione.Desc & """), Valida => False),");
         New_Line;
      end loop;
      Opzione := Le_Opzioni(Numero_Opzioni);
      Spazi_Bianchi(9);
      Put(Opzione.Nome & " => (Desc => T_U_S("""& Opzione.Desc & """), Valida => False) );");
      New_Line;
   end if;
   Put("      Risultati := ("); New_Line;
   for Cont in 1..Numero_Risultati - 1 loop
      Risl := I_Risultati(Cont);
      Spazi_Bianchi(6);
      Put(Risl.Nome &" => (Valr => " & Risl.Valr & ", Molt => " & Risl.Molt & ", Unit => T_U_S("""& Risl.Unit & """), Desc => T_U_S("""
	    & Risl.Desc & """)),");
      New_Line;
   end loop;
   Risl := I_Risultati(Numero_Risultati);
   Spazi_Bianchi(9);
   Put(Risl.Nome & " => (Valr => " & Risl.Valr & ", Molt => " & Risl.Molt & ", Unit => T_U_S("""& Risl.Unit & """), Desc => T_U_S("""
	 & Risl.Desc & """)) );");
   New_Line;
   Put("      Lettura_Dati_Opzioni;"); New_Line;
   for Cont in 1..Numero_Dati loop
      Dato := I_Dati(Cont);
      Spazi_Bianchi(6);
      Put(Dato.Nome & Suffx & " := Dati(" & Dato.Nome & ").Valr*Dati(" & Dato.Nome & ").Molt;");
      New_Line;
   end loop;
   if Numero_Opzioni > 0 then
      for Cont in 1..Numero_Opzioni loop
         Opzione := Le_Opzioni(Cont);
         Spazi_Bianchi(6);
         Put(Opzione.Nome & Suffx & " := Opzioni(" & Opzione.Nome & ").Valida;");
         New_Line;
      end loop;
   end if;
   Scrivi_Corpo(Iniziale => "      ", Inmiofloat => ",", Inboolean => ",", Finale => ");", Suffisso => Suffx);
   for Cont in 1..Numero_Risultati loop
      Risl := I_Risultati(Cont);
      Spazi_Bianchi(6);
      Put("Risultati(" & Risl.Nome & ").Valr := " & Risl.Nome & Suffx & "/Risultati(" & Risl.Nome & ").Molt;");
      New_Line;
   end loop;
   Spazi_Bianchi(6);
   Put("Scrittura;"); New_Line;
   Put("   end " & Filename & "_0;"); New_Line;
   Put("end LS." & Filename & ";"); New_Line;
   Put("with LS." & Filename & ";"); New_Line;
   Put("use LS." & Filename & ";"); New_Line;
   Put("procedure LS." & Filename & "_1 is begin"); New_Line;
   Put("   " & Filename & "_0;"); New_Line;   
   Put("end LS." & Filename & "_1;"); New_Line;
   Close(File_Ada);
   -- Scrive il file delle procedure comuni al programma a linea di comando ed a quello a finestre -----------------------------------------------
   Create(File_Corpo, Out_File, To_String(Filename) & ".Corpo.adx");
   Set_Output(File_Corpo);
   Put("separate(LS." & Filename & ")"); New_Line;
   Scrivi_Corpo(Iniziale => "   procedure", Inmiofloat => " : in Mio_Float;", Inboolean => " : in Boolean;", Finale => " : out Mio_Float) is ", 
		Suffisso => " ");
   Put("   begin"); New_Line;
   Put("--"); New_Line;
   Put("-- Corpo del calcolo"); New_Line;
   Put("--"); New_Line;
   Put("      null;"); New_Line;
   Put("   end Corpo;"); New_Line;
   -- Script per compilare il programma Ada a linea di comando -----------------------------------------------
   Close(File_Corpo);
   Create(File_Sh, Out_File, To_String(Filename) & ".shx");
   Set_Output(File_Sh);
   Put("#!"); New_Line;
   Put("ADA_LS=~/Luigi/Sviluppo/Ada_LS;"); New_Line;
   Put("gnatchop -w $ADA_LS/LS.ada;"); New_Line;
   Put("gnatchop -w $ADA_LS/definizioni.ada;"); New_Line;
   Put("gnatchop -w $ADA_LS/utilita.ada;"); New_Line;
   Put("gnatchop -w $ADA_LS/Gestione_IO/gestione_IO.ada;"); New_Line;
   Put("gnatchop -w $ADA_LS/solve.ada;"); New_Line;
   Put("gnatchop -w $ADA_LS/solve_gen.ada;"); New_Line;
   Put("gnatchop -w $ADA_LS/Derivate_Numeriche.ada;"); New_Line;
   Put("gnatchop -w $ADA_LS/vapore.ada;"); New_Line;
   Put("gnatchop -w $ADA_LS/interpolazione_curve.ada;"); New_Line;
   Put("gnatchop -w " & Filename & ".Corpo.ada;"); New_Line;
   Put("gnatchop -w " & Filename & ".ada;"); New_Line;
   Put("gnatmake -o " & Filename & "_0 ls-" & Filename & "_1; # -gnateE -bargs -E -g -largs"); New_Line;
   Put("rm ls.ads; rm ls-*.adb; rm ls-*.ads; rm *.o; rm *.ali;"); New_Line;
   Close(File_Sh);
   Create(File_Jewl, Out_File, To_String(Filename) & "_jewl.adx");
   -- Scrive l'abbozzo del programma Ada con finestre Jewl ------------------------------------------------------
   Set_Output(File_Jewl);
   Put("with Ada.Numerics; use Ada.Numerics;"); New_Line;
   Put("with Ada.Strings.Fixed; use Ada.Strings.Fixed;"); New_Line;
   Put("with Ada.Text_IO; use Ada.Text_IO;"); New_Line;
   Put("with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;"); New_Line;
   Put("with Ada.Calendar; use Ada.Calendar;"); New_Line;
   Put("with LS.Numerico; use LS.Numerico;"); New_Line;
   Put("with LS.Float_IO; use LS.Float_IO;"); New_Line;
   Put("with LS.Integer_IO; use LS.Integer_IO;"); New_Line;
   Put("with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;"); New_Line;
   Put("with LS.Utili; use LS.Utili;"); New_Line;
   Put("with LS.Solve1D; use LS.Solve1D;"); New_Line;
   Put("with LS.Solve1D_Gen;"); New_Line;
   Put("with LS.Vapore; use LS.Vapore;"); New_Line;
   Put("with LS.Flusso_Isentropico; use LS.Flusso_Isentropico;"); New_Line;
   Put("with LS.Pdc_Diff;"); New_Line;
   Put("with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;"); New_Line;
   Put("with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;"); New_Line;
   Put("with Ada.Exceptions;"); New_Line;
   Put("with LS.Utili; use LS.Utili;"); New_Line;
   Put("with JEWL.Simple_Windows; use JEWL.Simple_Windows;"); New_Line;
-- Put("with LS.LC_Gen;"); New_Line;
   Put("package LS." & Filename & " is"); New_Line;
-- Put("   type Dati_Inp is (Aaa); -- muti"); New_Line;
-- Put("   type Opz_Cal is (Bbb); "); New_Line;
-- Put("   type Ris_Out is (Ccc); "); New_Line;
-- Put("   package LC is new "); New_Line;
-- Put("     LS.LC_Gen(Dati_Input => Dati_Inp, Opzioni_Calcolo => Opz_Cal, Risultati_Output => Ris_Out);"); New_Line;
-- Put("   use LC;"); New_Line;
   Spazi_Bianchi(3);
   for Cont in 1..Numero_Dati - 1 loop
      Put(I_Dati(Cont).Nome & ", ");
   end loop;
   Put(I_Dati(Numero_Dati).Nome & " : Mio_Float;"); 
   New_Line;
   Spazi_Bianchi(3);
   for Cont in 1..Numero_Risultati - 1 loop
      Put(I_Risultati(Cont).Nome & ", ");
   end loop;
   Put(I_Risultati(Numero_Risultati).Nome & " : Mio_Float;"); 
   New_Line;
   if Numero_Opzioni > 0 then
      Spazi_Bianchi(3);
      for Cont in 1..Numero_Opzioni - 1 loop
         Put(Le_Opzioni(Cont).Nome & ", ");
      end loop;
      Put(Le_Opzioni(Numero_Opzioni).Nome & " : Boolean := False;"); 
      New_Line;
   end if;
   Put("   H_Win : constant Positive := 400; -- Altezza della finestra"); New_Line;
   Put("   W_Win : constant Positive := 1000; -- Larghezza della finestra"); New_Line;
   Put("   X_Lab : constant Positive := 10;  -- Ascissa dell'inizio delle etichette"); New_Line;
   Put("   X_Edit : constant Positive := 350;--    ""          ""   dei campi di introduzione"); New_Line;
   Put("   W_Lab : constant Positive := 800; -- Larghezza delle etichette"); New_Line;
   Put("   W_Edit : constant Positive := 100; --   ""      dei campi"); New_Line;
   Put("   X_Check : constant Positive := X_Edit + W_Edit + 50; -- Ascissa d'inizio delle opzioni );"); New_Line;
   Put("   Alt : constant Positive := 20;    -- Altezza dei campi"); New_Line;
   Put("   Y_Ini : constant Positive := 10;      -- Ordinata iniziale delle etichette"); New_Line;
   Put("   DY : constant Positive := 25;         -- Spostamenti verso il basso"); New_Line;
   Put("   Fin : Frame_Type := Frame(H_Win,W_Win,""Titolo della finestra"",'Q');"); New_Line;
   declare
      Kont : Natural;
      Risult : String := """RISULTATI""";
   begin
      for Cont in 1..Numero_Dati loop
         Dato := I_Dati(Cont);
         Spazi_Bianchi(3);
         Dato.Unit := " [" & Dato.Unit & "]";
         Put(
         "L_" & Dato.Nome & " : Label_Type := Label(Fin,(X_Lab,Y_Ini +" & Natural'Image(Cont - 1) & "*DY),W_Lab,Alt,""" & Dato.Desc & Dato.Unit & """); ");
         New_Line;
         Spazi_Bianchi(3);
         Put("E_" & Dato.Nome & " : Editbox_Type := Editbox(Fin,(X_Edit,Y_Ini +" & Natural'Image(Cont - 1) & "*DY),W_Edit,Alt,""" & Dato.Valr & """); ");
         New_Line;
         Kont := Cont + 1;
      end loop;
      Put("   Y_Ini_Ris : constant Positive := Y_Ini + " & Natural'Image(Kont) & "*DY; -- Ordinata iniziale dei risultati"); New_Line;
      Put("   L_Risultati  : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris),W_Lab,Alt," & Risult & ");"); New_Line;
      for Cont in 1..Numero_Risultati loop
         Risl := I_Risultati(Cont);
         Spazi_Bianchi(3);
         Put("L_" & Risl.Nome & " : Label_Type := Label(Fin,(X_Lab,Y_Ini_Ris +" & Natural'Image(Cont) & "*DY),W_Lab,Alt," & """ "");" );    
         New_Line;
         Kont := Kont + 1;
      end loop;
      Kont := 0;
      for Cont in 1..Numero_Opzioni loop
         Opzione := Le_Opzioni(Cont);
         Spazi_Bianchi(3);
         Put(
             "C_" & Opzione.Nome & " : Checkbox_Type := Checkbox(Fin,(X_Check,Y_Ini +" & Natural'Image(Kont) & "*DY),W_Lab,Alt,""" & Opzione.Desc & """ ); ");
         New_Line;
         Kont := Kont + 1;
      end loop;
--    Spazi_Bianchi(3);
--    Put("E_File : Editbox_Type := Editbox(Fin,(X_Check,Y_Ini + " & Natural'Image(Kont) & "*DY),4*W_Edit,Alt,""file_dei_risultati"");"); New_Line;
--    Kont := Kont + 1;
      Spazi_Bianchi(3);
      Put("Calc : Button_Type := Button(Fin,(X_Check,Y_Ini +" & Natural'Image(Kont) & "*DY),2*W_Edit,Alt,""Calcola"",'X');");
      New_Line;
--    Kont := Kont + 1;
--    Spazi_Bianchi(3);
--    Put("Fine : Button_Type := Button(Fin,(X_Check,Y_Ini +" & Natural'Image(Kont) & "*DY),2*W_Edit,Alt,""Fine"",'Q');");      
      New_Line;
   end;
   Put("   Testo : Unbounded_String;"); New_Line;
   Put("   Il_File : File_Type;"); New_Line;
   Put("   procedure " & Filename & "_0 ;"); New_Line;
   Put("end LS." & Filename & ";"); New_Line;
   Put("package body LS." & Filename & " is"); New_Line;
   Scrivi_Corpo(Iniziale => "   procedure", Inmiofloat => " : in Mio_Float;", Inboolean => " : in Boolean;", 
		Finale => " : out Mio_Float) is separate;", Suffisso => " ");
   Put("   procedure " & Filename & "_0 is"); New_Line;
   Put("      function Formatta(Numero : Mio_Float) return String is"); New_Line;
   Put("         Numero_Formattato : String(1..10);"); New_Line;
   Put("      begin"); New_Line;
   Put("         Put(Numero_Formattato, Numero, 4, 0);"); New_Line;
   Put("         return Numero_Formattato;"); New_Line;
   Put("      end Formatta;"); New_Line;
   Put("      procedure Applica_Moltiplicatori is"); New_Line;
   Put("      begin"); New_Line;
   for Cont in 1..Numero_Dati loop
      Dato := I_Dati(Cont);
      Spazi_Bianchi(9);
      Put(Dato.Nome & " := " & Dato.Molt & " * Mio_Float'Value(Get_Text(E_" & Dato.Nome & "));" );
      New_Line;
   end loop;
   Put("      end Applica_Moltiplicatori;"); New_Line;
   Put("   begin"); New_Line;
   Spazi_Bianchi(6);
   Put("while Valid(Fin) loop"); New_Line;
   Spazi_Bianchi(9);
   Put("case Next_Command is"); New_Line;
   Spazi_Bianchi(12);
   Put("when 'X' =>"); New_Line;
   Spazi_Bianchi(15);
   Put("Applica_Moltiplicatori;"); New_Line;
   for Cont in 1..Numero_Opzioni loop
      Opzione := Le_Opzioni(Cont);
      Spazi_Bianchi(15);
      Put(Opzione.Nome & " := Get_State(C_" & Opzione.Nome & ");" );
      New_Line;
   end loop;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(7);-------------------------------
   Put("if Esiste_Il_File(Get_Text(E_File)) then     -- Se esiste già un file dei dati (ed eventualmente risultati)"); New_Line;
   Put("null; --");
   Spazi_Bianchi(10);
   Put("Open(Il_File, In_File, Get_Text(E_File)); -- copia i dati dal file alla finestra"); New_Line;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(10);
   Put("for I in 1..2 loop "); New_Line;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(13);
   Put("Skip_Line(File => Il_File); "); New_Line;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(10);
   Put("end loop; "); New_Line;
   for Cont in 1..Numero_Dati loop
      Dato := I_Dati(Cont);
      Put("null; --"); -------------------------------     
      Spazi_Bianchi(10);
      Put("Get(Il_File, " & Dato.Nome & ");" ); New_Line;
      Put("null; --"); -------------------------------
      Spazi_Bianchi(10);
      Put("Skip_Line(Il_File);" ); New_Line;
      Put("null; --"); -------------------------------
      Spazi_Bianchi(10);
      Put("Testo := TO_UNBOUNDED_STRING(Formatta(" & Dato.Nome & ") );" );
      New_Line;
      Put("null; --"); -------------------------------
      Spazi_Bianchi(10);
      Put("Set_Text(E_" & Dato.Nome & ", To_String(Testo));" );
      New_Line;
   end loop;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(10);
   Put("Close(Il_File);"); New_Line;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(10);
   Put("Applica_Moltiplicatori;"); New_Line;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(7);
   Put("else"); New_Line;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(10);
   Put("Create(Il_File, Out_File, Get_Text(E_File)); -- altrimenti ne crea uno nuovo"); New_Line;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(10);
   Put("Set_Output(Il_File);"); New_Line;   
   Put("null; --"); -------------------------------
   Spazi_Bianchi(10);
   Put("Put(""Dati del calcolo:"" ); New_Line;" ); New_Line;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(10);
   Put("Put("".................""); New_Line;" ); New_Line;
   for Cont in 1..Numero_Dati loop
      Dato := I_Dati(Cont);
      Put("null; --"); -------------------------------
      Spazi_Bianchi(10);
      Put("Put(Mio_Float'Value(Get_Text(E_" & Dato.Nome & ")), 3, 3, 0); " );
      Put("Put("" ; ["); Put(Dato.Unit); Put( "] " ); Put(Dato.Desc); Put( " "" ); New_Line;"); 
      New_Line;
   end loop;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(10);
   Put("Close(Il_File);"); New_Line;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(7);
   Put("end if;"); New_Line;
   Put("--"); New_Line;
   Spazi_Bianchi(14);
   Scrivi_Corpo(Iniziale => " ", Inmiofloat => ",", Inboolean => ",", Finale => ");", Suffisso => " ");
   Put("--"); New_Line;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(7);
   Put("Open(Il_File, Append_File, Get_Text(E_File));"); New_Line;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(7);
   Put("Set_Output(Il_File);"); New_Line;
   Spazi_Bianchi(15);
   Put("Put(""Risultati del calcolo:"" ); New_Line;" ); New_Line;
   Spazi_Bianchi(15);
   Put("Put("".....................""); New_Line;" ); New_Line;
   for Cont in 1..Numero_Risultati loop
      Risl := I_Risultati(Cont);
      Spazi_Bianchi(15);
      Put("Testo := TO_UNBOUNDED_STRING(""" & Risl.Desc & " [" & Risl.Unit & "] = """ & " & " & "Formatta(" & Risl.Nome & " /" & Risl.Molt & ") );" );
      New_Line;
      Spazi_Bianchi(15);
      Put("Put(Testo); New_Line;"); New_Line;
      Spazi_Bianchi(15);      
      Put("Set_Text(L_" & Risl.Nome & ", To_String(Testo));" );
      New_Line;
   end loop;
   Put("null; --"); -------------------------------
   Spazi_Bianchi(7);
   Put("Close(File => Il_File);" ); New_Line;
   Spazi_Bianchi(12);
   Put("when 'Q' => "); New_Line;
   Spazi_Bianchi(15);
   Put("exit;"); New_Line;
   Spazi_Bianchi(12);
   Put("when others => null;"); New_Line;
   Spazi_Bianchi(9);
   Put("end case;"); New_Line;
   Spazi_Bianchi(6);
   Put("end loop;"); New_Line;
   Spazi_Bianchi(3);
   Put("end " & Filename & "_0;"); New_Line;
   Put("end LS." & Filename & ";"); New_Line;
   Put("with LS." & Filename & ";"); New_Line;
   Put("use LS." & Filename & ";"); New_Line;
   Put("procedure LS." & Filename & "_1 is begin"); New_Line;
   Put("   " & Filename & "_0;"); New_Line;   
   Put("end LS." & Filename & "_1;"); New_Line;
   Close(File_Jewl);
   -- Script per compilare il programma Ada a finestre Jewl -----------------------------------------------
   Create(File_Jewl_Sh, Out_File, To_String(Filename) & "_jewl.shx");
   Set_Output(File_Jewl_Sh);
   Put("#!"); New_Line;
   Put("ADA_LS=~/Luigi/Sviluppo/Ada_LS;"); New_Line;
   Put("gnatchop -w $ADA_LS/LS.ada;"); New_Line;
   Put("gnatchop -w $ADA_LS/definizioni.ada;"); New_Line;
   Put("gnatchop -w $ADA_LS/utilita.ada;"); New_Line;
   Put("gnatchop -w $Ada_LS/solve.ada"); New_Line;
   Put("gnatchop -w $Ada_LS/solve_gen.ada"); New_Line;
   Put("gnatchop -w $Ada_LS/Derivate_Numeriche.ada"); New_Line;
   Put("gnatchop -w $Ada_LS/vapore.ada"); New_Line;
   Put("gnatchop -w $Ada_LS/interpolazione_curve.ada"); New_Line;
   Put("gnatchop -w $ADA_LS/Gestione_IO/gestione_IO.ada;"); New_Line;
   Put("gnatchop -w " & Filename & ".Corpo.ada;"); New_Line;
   Put("gnatchop -w $ADA_LS/jewl.ada;"); New_Line;
   Put("gnatchop -w " & Filename & "_jewl.ada;"); New_Line;
   Put("wine gnatmake -o " & Filename & "_0 ls-" & Filename & "_1; # -gnateE -bargs -E -g -largs"); New_Line;
   Put("rm ls.ads; rm ls-*.adb; rm ls-*.ads; rm *.o; rm *.ali;"); New_Line;
   Put("rm jewl*.adb; rm jewl*.ads;"); New_Line;
   Close(File_Jewl_Sh);
end LS.Prepara;
with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with LS.Integer_IO; use LS.Integer_IO;
with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;
with LS.Utili; use LS.Utili;
with LS.LC_Gen;
procedure LS.Provalc is 
   Pres1, Dens1, Diam1 : Mio_Float; -- LINEA VARIABILE
   Volum1, Kpa1 : Boolean := False;                         -- LINEA VARIABILE      
   type Dati_Inp is (Pres, Dens, Diam);               -- LINEA VARIABILE      
   type Opz_Cal is (Volum, Kpa );                           -- LINEA VARIABILE      
   type Ris_Out is (Port, Port_Vol);                  -- LINEA VARIABILE      
   package LC is new 
     LS.LC_Gen(Dati_Input => Dati_Inp, Opzioni_Calcolo => Opz_Cal, Risultati_Output => Ris_Out);
   use LC;
begin
   Dati := (Pres => (Valr => 1.0, Molt => 1.0E5, Unit => T_U_S("bar"), Desc => T_U_S("Pressione")),            -- LINEA VARIABILE
            Dens => (Valr => 1000.0, Molt => 1.0, Unit => T_U_S("kg/mc"), Desc => T_U_S("Densità")),           -- LINEA VARIABILE
            Diam => (Valr => 3.0, Molt => 0.001, Unit => T_U_S("mm"), Desc => T_U_S("Diametro")) );            -- LINEA VARIABILE
   Opzioni := (Kpa => (Desc => T_U_S("Pressione in kPa" ), Valida => False),
               Volum => (Desc => T_U_S("Calcola anche la portata volumica" ), Valida => False) );              -- LINEA VARIABILE
   Risultati := (Port => (Valr => 0.0, Molt => 3600.0, Unit => T_U_S("kg/h"), Desc => T_U_S("Portata")),                -- LINEA VARIABILE
                 Port_Vol => (Valr => 0.0, Molt => 3600.0, Unit => T_U_S("mc/h"), Desc => T_U_S("Portata volumica")) ); -- LINEA VARIABILE
   Lettura_Dati_Opzioni;
   Pres1 := Dati(Pres).Valr*Dati(Pres).Molt; -- LINEA VARIABILE
   Dens1 := Dati(Dens).Valr; -- LINEA VARIABILE
   Diam1 := Dati(Diam).Valr*Dati(Diam).Molt; -- LINEA VARIABILE
   Volum1 := Opzioni(Volum).Valida; -- LINEA VARIABILE
   KPa1 := Opzioni(Kpa).Valida;
   if Kpa1 then
      Pres1 := 1000.0*Dati(Pres).Valr;
      Dati(Pres).Unit := T_U_S("kPa");
   end if;
   declare
      Portata : Mio_Float := Ar_Cer(Diam1)*0.61*Sqrt(2.0*Pres1*Dens1);
   begin
      Risultati(Port).Valr := Portata*Risultati(Port).Molt;
      if Volum1 then
         Risultati(Port_Vol).Valr := Portata*Risultati(Port_Vol).Molt/Dens1;
      end if;
   end;
   Scrittura;
end LS.Provalc;

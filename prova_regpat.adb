with Ada.Text_IO;
with GNAT.Regpat;

procedure Prova_Regpat is
   Nodi : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile( "nodi\s+(\d*)\s+(\d*)\s+(\d*)", GNAT.Regpat.Case_Insensitive );
   Nodo_1 : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile( "nodo1\s+\d+", GNAT.Regpat.Case_Insensitive );
   Nodo_2 : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile( "(nodo2\s+\d+)", GNAT.Regpat.Case_Insensitive );
   Num_Nodo_1 : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile( "(\s+\d+)" );
   Num_Nodo_2 : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile( "(\s+\d+)" );
   Numero_Dec : String := "(\s+[+-]?\d*\.\d*[eE]?[+-]?\d*)";
   Valore : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile( "capac" & Numero_Dec, GNAT.Regpat.Case_Insensitive );
   Esempio : constant String := "nodo1   23 -120.0e-12 Nodo2  4 nodi    23  12   32 capac 560.0e-12";
   Risult : GNAT.Regpat.Match_Array(0..3);
begin
   GNAT.Regpat.Match(Nodi, Esempio, Risult); 
   Ada.Text_IO.Put_Line(Esempio(Risult(0).First..Risult(0).Last ));
   Ada.Text_IO.Put_Line(Esempio(Risult(1).First..Risult(1).Last ));
   Ada.Text_IO.Put_Line(Esempio(Risult(2).First..Risult(2).Last ));
   Ada.Text_IO.Put_Line(Esempio(Risult(3).First..Risult(3).Last ));
   GNAT.Regpat.Match(Num_Nodo_1, Esempio, Risult); 
   Ada.Text_IO.Put_Line(Esempio(Risult(0).First..Risult(0).Last ));
   Ada.Text_IO.Put_Line(Esempio(Risult(1).First..Risult(1).Last ));
   Ada.Text_IO.Put_Line(Esempio(Risult(1).First..Risult(1).Last ));
   GNAT.Regpat.Match(Nodo_2, Esempio, Risult); 
   Ada.Text_IO.Put_Line(Esempio(Risult(0).First..Risult(0).Last ));
   Ada.Text_IO.Put_Line(Esempio(Risult(1).First..Risult(1).Last ));
   Ada.Text_IO.Put_Line(Esempio(Risult(1).First..Risult(1).Last ));
   GNAT.Regpat.Match(Valore, Esempio, Risult); 
   Ada.Text_IO.Put_Line(Esempio(Risult(0).First..Risult(0).Last ));
   Ada.Text_IO.Put_Line(Esempio(Risult(1).First..Risult(1).Last ));
   GNAT.Regpat.Match(GNAT.Regpat.Compile(Numero_Dec), Esempio, Risult); 
   Ada.Text_IO.Put_Line(Esempio(Risult(0).First..Risult(0).Last ));
end Prova_Regpat;

   

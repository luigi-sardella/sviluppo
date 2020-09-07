with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics; use Ada.Numerics;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.Regpat;
with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with LS.Integer_IO; use LS.Integer_IO;
with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;
package LS.utili is -- Definizioni varie per i programmi di CFD
   type Funzione is access function(X : Mio_Float) return Mio_Float;
   type Vettore is array(Positive range <>) of Mio_Float;
   type Matrice is array(Positive range <>, Positive range <>) of Mio_Float;
   type Valore_Indice is record
      Valore : Mio_Float;
      Indice : Integer;
   end record;
   type Gnu_Plot is (Gnuplot, Nognuplot);
   Non_Nell_Intervallo : exception;
   function "*"(V, W : Vettore) return Vettore;
   function "*"(A : Mio_Float; V : Vettore) return Vettore;
   function "*"(V : Vettore; B : Mio_Float) return Vettore;
   function "*"(M : Matrice; V : Vettore) return Vettore;
   function "+"(V, W : Vettore) return Vettore;
   function "+"(A : Mio_Float; V : Vettore) return Vettore;
   function "+"(V : Vettore; B : Mio_Float) return Vettore;
   function "-"(V, W : Vettore) return Vettore;
   function "-"(A : Mio_Float; V : Vettore) return Vettore;
   function "-"(V : Vettore; B : Mio_Float) return Vettore;
   function "-"(V : Vettore) return Vettore;
   function "/"(V, W : Vettore) return Vettore;
   function "/"(V : Vettore; B : Mio_Float) return Vettore;
   function "/"(A : Mio_Float; V : Vettore) return Vettore;
   function "**"(V : Vettore; N : Integer) return Vettore;
   function "**"(V : Vettore; N : Mio_Float) return Vettore;
   function "Abs"(V : Vettore) return Vettore;
   function Modulo(V : Vettore) return Mio_Float;
   function Map(V : Vettore; F:Funzione) return Vettore;
   function Log(V : Vettore) return Vettore;
   function Exp(V : Vettore) return Vettore;
   function Sqrt(V : Vettore) return Vettore;
   function Max(A,B : Mio_Float) return Mio_Float;
   function Max(V : Vettore) return Mio_Float;
   function Max(V, W : Vettore) return Vettore;
   function Max(V : Vettore; B : Mio_Float) return Vettore;
   function Max(A : Mio_Float; V : Vettore) return Vettore;
   function Max(V : Vettore) return Valore_Indice;
   function Min(A,B : Mio_Float) return Mio_Float;
   function Min(V : Vettore; B : Mio_Float) return Vettore;
   function Min(V, W : Vettore) return Vettore;
   function Min(A : Mio_Float; V : Vettore) return Vettore;
   function Min(V : Vettore) return Mio_Float;
   function Min(V : Vettore; Ma_Piu_Di : Mio_Float) return Mio_Float;
   function Min(V : Vettore) return Valore_Indice;
   function Signum(A, B : Mio_Float) return Mio_Float;
   function Sum(V : Vettore) return Mio_Float;
   function Dot(V, W : Vettore) return Mio_Float;
   function Df_A(V : Vettore) return Vettore; -- Differenza avanti
   function Df_I(V : Vettore) return Vettore; -- Differenza indietro
   function Df_Simm(V : Vettore) return Vettore; -- Differenza simmetrica
   function Piu_Un_Mezzo(V : Vettore) return Vettore; -- Vettore interpolato a i + 1/2
   function Meno_Un_Mezzo(V : Vettore) return Vettore; -- Vettore interpolato a i - 1/2
   function Df_P(V : Vettore) return Vettore renames Df_A;
   function Df_M(V : Vettore) return Vettore renames Df_I;
   function Df_S(V : Vettore) return Vettore renames Df_Simm;
   function PUM(V : Vettore) return Vettore renames Piu_Un_Mezzo;
   function MUM(V : Vettore) return Vettore renames Meno_Un_Mezzo;
   procedure Put(V : in Vettore );
   procedure Put(S : in String; V : in Vettore);
   procedure Stampa_Curve(X,Y : in Vettore; Nome_file : in String := "";
                          Max_Ind : in Natural := 0; Gnplt : Gnu_Plot := Gnuplot);
   procedure Stampa_Curve(X,Y1,Y2 : in Vettore; Nome_file : in String := "");
   procedure Stampa_Curve(X,Y1,Y2,Y3 : in Vettore; Nome_file : in String := "");
   function Simpson(X,F : Vettore) return Mio_Float;
   function Simpson(X,F : Vettore) return Vettore;
   function Integra(X,F : Vettore) return Mio_Float renames Simpson;
   function Integra(X,F : Vettore) return Vettore renames Simpson;
   function Thomas(Sx,Diag,Dx,Tn : Vettore) return Vettore;
   function Prova_Thomas(Sx,Diag,Dx,Tn,X : Vettore) return Vettore;
   function Interpola(X1, Y1, X2, Y2, X : Mio_Float) return Mio_Float;
   function Interpola(Xv, Yv : Vettore; X : Mio_float) return Mio_Float;
   function Interpola(Xv, Yv, Vx : Vettore) return Vettore;
   function Ar_Cer(Dia : Mio_Float) return Mio_Float;
   function Dia_Cer(Ar : Mio_Float) return Mio_Float;
   function Pre_Def(Pr_D : Mio_Float; Kont : Positive) return Mio_Float;
   function Pre_Def(Pr_D, Kont : Positive) return Positive;
   function Esiste_Il_File(File_Name : in String) return Boolean;
   function DND(Q, V : Mio_Float) return Positive;
   function Leggi_Dato(Dati : String; Nome : String; Predefinito : Mio_Float; Descrizione : String := " ") return Mio_Float;
   function Leggi_Dato(Dati : String; Nome : String; Predefinito : Positive; Descrizione : String := " ") return Positive;
end LS.Utili;
package body LS.utili is
   function "*"(V, W : Vettore) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := V(i) * W(i);
      end loop;
      return C;
   end "*";
   function "*"(V : Vettore; B : Mio_Float) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := V(i) * B;
      end loop;
      return C;
   end "*";
   function "*"(A : Mio_Float; V : Vettore) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := A * V(i);
      end loop;
      return C;
   end "*";
   function "*"(M : Matrice; V : Vettore) return Vettore is
      C : Vettore := (V'range => 0.0);
   begin
      for I in V'Range loop
         for J in V'Range loop
            C(I) := C(I) + M(I,J) * V(J);
         end loop;
      end loop;
      return C;
   end "*";
   function "+"(V, W : Vettore) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := V(i) + W(i);
      end loop;
      return C;
   end "+";
   function "+"(V : Vettore; B : Mio_Float) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := V(i) + B;
      end loop;
      return C;
   end "+";
   function "+"(A : Mio_Float; V : Vettore) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := A + V(i);
      end loop;
      return C;
   end "+";
   function "-"(V, W : Vettore) return Vettore is
      C : Vettore := (V'Range => 0.0);
      Vd, Wd : Mio_Double;
   begin
      for i in V'Range loop
         Vd := Mio_Double(V(I));
         Wd := Mio_Double(W(I));
         C(i) := Mio_Float(Vd - Wd);
      end loop;
      return C;
   end "-";
   function "-"(V : Vettore; B : Mio_Float) return Vettore is
      C : Vettore := (V'Range => 0.0);
      Vd : Mio_Double;
      Bd : Mio_Double := Mio_Double(B);
   begin
      for i in V'Range loop
         Vd := Mio_Double(V(I));
         C(i) := Mio_Float(Vd - Bd);
      end loop;
      return C;
   end "-";
   function "-"(A : Mio_Float; V : Vettore) return Vettore is
      C : Vettore := (V'Range => 0.0);
      Vd : Mio_Double;
      Ad : Mio_Double := Mio_Double(A);
   begin
      for i in V'Range loop
         Vd := Mio_Double(V(I));
         C(i) := Mio_Float(Ad - Vd);
      end loop;
      return C;
   end "-";
   function "-"(V : Vettore) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := - V(i);
      end loop;
      return C;
   end "-";
   function "/"(V, W : Vettore) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := V(i) / W(i);
      end loop;
      return C;
   end "/";
   function "/"(V : Vettore; B : Mio_Float) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := V(i) / B;
      end loop;
      return C;
   end "/";
   function "/"(A : Mio_Float; V : Vettore) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := A / V(i);
      end loop;
      return C;
   end "/";
   function "**"(V : Vettore; N : Integer) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := V(i) ** N;
      end loop;
      return C;
   end "**";
   function "**"(V : Vettore; N : Mio_Float) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := V(i) ** N;
      end loop;
      return C;
   end "**";
   function "Abs"(V : Vettore) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := Abs( V(i) );
      end loop;
      return C;
   end "Abs";
   function Modulo(V : Vettore) return Mio_Float is
   begin
      return Sqrt(Dot(V, V));
   end Modulo;
   function Map(V : Vettore; F:Funzione) return Vettore is
      C : Vettore := (V'Range => 0.0);
   begin
      for i in V'Range loop
         C(i) := F( V(i) );
      end loop;
      return C;
   end Map;
   function Log(V : Vettore) return Vettore is
   begin
      return Map(V, Log'access);
   end Log;
   function Exp(V : Vettore) return Vettore is
   begin
      return Map(V, Exp'Access);
   end Exp;
   function Sqrt(V : Vettore) return Vettore is
   begin
      return Map(V, Sqrt'access);
   end Sqrt;
   function Max(V : Vettore) return Mio_Float is
      M : Mio_Float := V(V'First);
      M1 : Mio_Float;
   begin
      for i in V'First+1..V'Last loop
         M1 := V(i);
         if M1 > M then
            M := M1;
         end if;
      end  loop;
      return M;
   end Max;
   function Max(V, W : Vettore) return Vettore is
      V1 : Vettore := V;
   begin
      for i in V'range loop
         V1(I) := Max(V1(I), W(I));
      end  loop;
      return V1;
   end Max;
   function Max(V : Vettore) return Valore_Indice is
      -- fornisce non solo il valore del massimo, ma anche
      -- l'indice corrispondente
      M : Valore_Indice := (Valore =>V(V'First), Indice => V'First);
      M1 : Mio_Float;
   begin
      for i in V'First+1..V'Last loop
         M1 := V(i);
         if M1 > M.Valore then
            M.Valore := M1;
            M.Indice := I;
         end if;
      end  loop;
      return M;
   end Max;
   function Max(V : Vettore; B : Mio_Float) return Vettore is
      C : Vettore := (V'range => 0.0);
   begin
      for i in V'range loop
         C(I) := Max(V(I),B);
      end  loop;
      return C;
   end Max;
   function Max(A : Mio_Float; V : Vettore) return Vettore is
      C : Vettore := (V'range => 0.0);
   begin
      for i in V'range loop
         C(I) := Max(V(I),A);
      end  loop;
      return C;
   end Max;
   function Max(A,B : Mio_float) return Mio_Float is
   begin
      if A > B then
         return A;
      else
         return B;
      end if;
   end Max;
   function Min(V : Vettore) return Mio_Float is
   begin
      return -Max(-V);
   end Min;
   function Min(V, W : Vettore) return Vettore is
   begin
      return -Max(-V, -W);
   end Min;
   function Min(V : Vettore; Ma_Piu_Di : Mio_Float) return Mio_Float is
      M, M1 : Mio_Float;
      Ind : Natural := 0;
      W : Vettore := (V'Range => 0.0);
   begin
      for I in V'Range loop
         M := V(I);
         if M > Ma_Piu_Di then
            Ind := Ind + 1;
            W(Ind) := M;
         end if;
      end loop;
      M := W(1);
      for i in 2..Ind loop
         M1 := W(i);
         if M1 < M Then
            M := M1;
         end if;
      end  loop;
      return M;
   end Min;
   function Min(V : Vettore; B : Mio_Float) return Vettore is
   begin
      return -Max(-V, -B);
   end Min;
   function Min(A : Mio_Float; V : Vettore) return Vettore is
   begin
      return -Max(-A, -V);
   end Min;
   function Min(V : Vettore) return Valore_Indice is
      -- fornisce non solo il valore del minimo, ma anche
      -- l'indice corrispondente
      M : Valore_Indice := (Valore =>V(V'First), Indice => V'First);
      M1 : Mio_Float;
   begin
      for i in V'First+1..V'Last loop
         M1 := V(i);
         if M1 < M.Valore then
            M.Valore := M1;
            M.Indice := I;
         end if;
      end  loop;
      return M;
   end Min;
   function Min(A,B : Mio_float) return Mio_Float is
   begin
      if A > B then
         return B;
      else
         return A;
      end if;
   end Min;
   function Signum(A, B : Mio_Float) return Mio_Float is
   begin
     if B > 0.0 then
       return abs(A);
     elsif B < 0.0 then
       return -abs(A);
     else
       return 0.0;
     end if;
   end Signum;
   function Sum(V : Vettore) return Mio_Float is
      M : Mio_Float := 0.0;
   begin
      for i in V'range loop
         M := M + V(i);
      end  loop;
      return M;
   end Sum;
   function Dot(V, W : Vettore) return Mio_Float is
   begin
      return Sum(V * W);
   end Dot;
   function Df_A(V : Vettore) return Vettore is -- Differenza in avanti
      C : Vettore := (V'Range => 0.0);
      First : Positive := V'First;
      Last : Positive := V'Last;
   begin
      for i in First..Last-1 loop
         C(i) := Mio_Float(Mio_Double(V(I+1)) - Mio_Double(V(I)));
      end loop;
      return C;
   end Df_A;
   function Df_I(V : Vettore) return Vettore is -- Differenza all'indietro
      C : Vettore := (V'Range => 0.0);
      First : Positive := V'First;
      Last : Positive := V'Last;
   begin
      for i in First+1..Last loop
         C(i) := Mio_Float(Mio_Double(V(I)) - Mio_Double(V(I-1)));
      end loop;
      return C;
   end Df_I;
   function Df_Simm(V : Vettore) return Vettore is -- Differenza simmetrica
      C : Vettore := (V'Range => 0.0);
      First : Positive := V'First;
      Last : Positive := V'Last;
   begin
      for i in First+1..Last-1 loop
         C(i) := Mio_Float(Mio_Double(V(I+1)) - Mio_Double(V(I-1)));
      end loop;
      return C;
   end Df_Simm;
   function Df_P(V, W : Vettore) return Vettore is -- Differenza in avanti
      C : Vettore := (V'Range => 0.0);
      First : Positive := V'First;
      Last : Positive := V'Last;
   begin
      for i in First..Last-1 loop
         C(i) := Mio_Float(Mio_Double(V(I+1)) - Mio_Double(W(I)));
      end loop;
      return C;
   end Df_P;
   function Df_M(V, W : Vettore) return Vettore is -- Differenza all'indietro
      C : Vettore := (V'Range => 0.0);
      First : Positive := V'First;
      Last : Positive := V'Last;
   begin
      for i in First+1..Last loop
         C(i) := Mio_Float(Mio_Double(V(I-1)) - Mio_Double(W(I)));
      end loop;
      return C;
   end Df_M;
   function Meno_Un_Mezzo(V : Vettore) return Vettore is -- Vettore interpolato a i - 1/2
      C : Vettore := (V'Range => 0.0);
      First : Positive := V'First;
      Last : Positive := V'Last;
   begin
      for i in First+1..Last-1 loop
         C(i) := (V(i) + V(i-1)) / 2.0;
      end loop;
      return C;
   end Meno_Un_Mezzo;
   function Piu_Un_Mezzo(V : Vettore) return Vettore is -- Vettore interpolato a i + 1/2
      C : Vettore := (V'Range => 0.0);
      First : Positive := V'First;
      Last : Positive := V'Last;
   begin
      for i in First+1..Last-1 loop
         C(i) := (V(i+1) + V(i)) / 2.0;
      end loop;
      return C;
   end Piu_Un_Mezzo;
   procedure Put(V : in Vettore) is
   begin
      for I in V'Range loop
         Put(V(I)); Put(" ");
      end loop;
      New_Line;
   end Put;
   procedure Put(S : in String; V : in Vettore) is
   begin
      New_Line;
      Put(S);
      New_Line;
      Put(V);
      New_Line;
   end Put;
   procedure Stampa_Curve(X,Y : in Vettore; Nome_file : in String := "";
                          Max_Ind : in Natural := 0; Gnplt : Gnu_Plot := Gnuplot) is
      Nuovo_file : File_Type;
      Last : Natural;
   begin
      if Nome_File = "" then
         Set_Output(Standard_Output);
      else
         Create(Nuovo_File,Out_File,Nome_File);
         Set_Output(Nuovo_File);
      end  if;
      if Gnplt = Gnuplot then
         Put("set term postscript color"); New_Line;
         Put("set output '"); Put(Nome_File); Put(".ps'"); New_Line;
         Put("plot'-' title '"); Put(Nome_File); Put("' with lines"); New_Line;
      end if;
      if Max_Ind = 0 then
         Last := X'Last;
      else
         Last := Max_Ind;
      end if;
      for i in X'First..Last loop
         Put(X(i)); Put("   "); Put(Y(i));
         New_Line;
      end loop;
      if Nome_File /= "" then
         Close(Nuovo_File);
         Set_Output(Standard_Output);
      end if;
   end Stampa_Curve;
   procedure Stampa_Curve(X,Y1,Y2 : in Vettore; Nome_file : in String := "") is
      Nuovo_file : File_Type;
   begin
      if Nome_File = "" then
         Set_Output(Standard_Output);
      else
         Create(Nuovo_File,Out_File,Nome_File);
         Set_Output(Nuovo_File);
      end  if;
      for i in X'Range loop
         Put(X(i)); Put("   "); Put(Y1(i)); Put("  "); Put(Y2(i));
         New_Line;
      end loop;
      if Nome_File /= "" then
         Close(Nuovo_File);
         Set_Output(Standard_Output);
      end if;
   end Stampa_Curve;
   procedure Stampa_Curve(X,Y1,Y2,Y3 : in Vettore; Nome_file : in String := "") is
      Nuovo_file : File_Type;
   begin
      if Nome_File = "" then
         Set_Output(Standard_Output);
      else
         Create(Nuovo_File,Out_File,Nome_File);
         Set_Output(Nuovo_File);
      end  if;
      for i in X'Range loop
         Put(X(i)); Put("   "); Put(Y1(i)); Put("  "); Put(Y2(i));
         Put("  "); Put(Y3(i));
         New_Line;
      end loop;
      if Nome_File /= "" then
         Close(Nuovo_File);
         Set_Output(Standard_Output);
      end if;
   end Stampa_Curve;
   function Simpson(X,F : Vettore) return Mio_float is
      -- Integra F rispetto ad X
      S : Mio_Float := 0.0;
   begin
      for I in X'First..X'Last-1 loop
         S := S + 0.5*(F(I) + F(I+1))*(X(I+1) - X(I));
      end loop;
      return S;
   end Simpson;
   function Simpson(X,F : Vettore) return Vettore is
      -- Integra F rispetto ad X
      V : Vettore := X;
   begin
      V(V'First) := 0.0;
      for I in X'First+1..X'Last loop
         V(I) := V(I-1) + 0.5*(F(I-1) + F(I))*(X(I) - X(I-1));
      end loop;
      return V;
   end Simpson;
   function Thomas(Sx,Diag,Dx,Tn : Vettore) return Vettore is
      -- Sx : coefficienti a sinistra della diagonale
      -- Dx : coefficienti a destra della diagonale
      -- Diag : coefficienti sulla diagonale
      -- Tn : vettore dei termini noti
      R : Mio_Float;
      J : Positive;
      Iu : Positive := Sx'Last;
      Tn1 : Vettore := Tn;
      Diag1 : Vettore := Diag;
   begin
      for I in 2..Iu loop
         R := Dx(I) / Diag1(I-1);
         Diag1(I) := Diag1(I) - R*Sx(I-1);
         Tn1(I) := Tn1(I) - R*Tn1(I-1);
      end loop;
      Tn1(Iu) := Tn1(Iu)/Diag1(Iu);
      for I in 2..Iu loop
         J := Iu - I + 1;
         Tn1(J) := (Tn1(J) - Sx(J)*Tn1(J+1))/Diag1(J);
      end loop;
      return Tn1;
   end Thomas;
   function Prova_Thomas(Sx,Diag,Dx,Tn,X : Vettore) return Vettore is
      -- dovrebbe restituire il vettore nullo
      W : Vettore := X;
      F : Positive := W'First;
      L : Positive := W'Last;
   begin
      W(F) := Diag(F)*X(F) + Sx(F)*X(F+1) - Tn(F);
      W(L) := Dx(L)*X(L-1) + Diag(L)*X(L) - Tn(L);
      for I in F+1..L-1 loop
         W(I) := Dx(I)*X(I-1) + Diag(I)*X(I) + Sx(I)*X(I+1) - Tn(I);
      end loop;
      return W;
   end Prova_Thomas;
   function Interpola(X1, Y1, X2, Y2, X : Mio_Float) return Mio_Float is
      Y : Mio_Float := Y1 + (X - X1)*(Y2 - Y1)/(X2 - X1);
   begin
      return Y;
   end Interpola;
   function Interpola(Xv, Yv : Vettore; X : Mio_float) return Mio_float is
      -- Interpolazione lineare
      type Paio is record
         I,J : Positive;
      end record;
      Lung : Positive := 2; -- Ci vogliono almeno due termini per interpolare
      Punti : Paio; -- := (1, Lung);
--    Non_In_Ordine,Non_Nell_Intervallo,Lunghezze_Differenti: Exception;
      Non_In_Ordine,Lunghezze_Differenti: Exception;
      function Bisez(Cp : Paio) return Paio is
         Ris : Paio := Cp;
         K : Positive;
      begin
         if Cp.I >= Cp.J then raise Non_In_Ordine; end if;
         if X < Xv(Cp.I) or X > Xv(Cp.J) then
            New_Line; Put(" X : "&Mio_Float'Image(X));
            New_Line; Put(" X("&Positive'Image(Cp.I)&") : "&Mio_Float'Image(Xv(Cp.I)));
            New_Line; Put(" X("&Positive'Image(Cp.J)&") : "&Mio_Float'Image(Xv(Cp.J)));
            raise Non_Nell_Intervallo;
         end if;
         if Cp.J - Cp.I > 1 then
            K := (Cp.I + Cp.J)/2;
            if Xv(Cp.I) <= X and X < Xv(K) then
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
      for I in 3..Xv'Last loop
         exit when Xv(I) = 0.0 and
           (Xv(I-1) = 0.0 or (Xv(I-1) < 0.0 and Xv(I-2) > Xv(I-1))
                          or (Xv(I-1) > 0.0 and Xv(I-2) < Xv(I-1)));
         Lung := Lung + 1;
      end loop;
      Punti := (1, Lung);
      Punti := Bisez(Punti);
      return Interpola(Xv(Punti.I), Yv(Punti.I), Xv(Punti.J), Yv(Punti.J), X);
   end Interpola;
   function Interpola(Xv, Yv, Vx : Vettore) return Vettore is
      First : Natural := Xv'First;
      Last : Natural := Xv'Last;
      Vy : Vettore(Xv'Range) := (others => 0.0);
   begin
      for I in First+1..Last-1 loop
         Vy(I) := Interpola(Xv, Yv, Vx(I));
      end loop;
      return Vy;
   end Interpola;
   function Ar_Cer(Dia : Mio_Float) return Mio_Float is
   begin
      return Dia**2*Pi/4.0;
   end Ar_Cer;
   function Dia_Cer(Ar : Mio_Float) return Mio_Float is
   begin
      return Sqrt(Ar*4.0/Pi);
   end Dia_Cer;
   function Pre_Def(Pr_D : Mio_Float; Kont : Positive) return Mio_Float is
   begin
      if Argument_Count < Kont then
         return Pr_D;
      end if;
      if Argument(Kont) = "-" then
         return Pr_D;
      else
         return Mio_Float'Value(Argument(Kont));
      end if;
   end Pre_Def;
   function Pre_Def(Pr_D, Kont : Positive) return Positive is
   begin
      if Argument_Count < Kont then
         return Pr_D;
      end if;
      if Argument(Kont) = "-" then
         return Pr_D;
      else
         return Positive'Value(Argument(Kont));
      end if;
   end Pre_Def;
   function Esiste_Il_File(File_Name : in String) return Boolean is
      F      : File_Type;
      Answer : Boolean := True;
   begin
      begin
         Open(F, In_File, File_Name);
         Close(F);
      exception
         when Name_Error => Answer := False;
      end;
      return Answer;
   end Esiste_Il_File; 
   function DND(Q, V : Mio_Float) return Positive is
      DN : array(Positive range <>) of Positive := (15, 20, 25, 32, 40, 50, 65, 80, 100, 125, 150, 200, 250, 300, 350, 
                                                    400, 450, 500);
      Scarti : Vettore(DN'range);
      Piu_Vicina : Valore_Indice; -- record (valore, indice)
   begin -- sceglie il diametro nominale che dia la velocità più vicina a quella indicata
      for I in DN'Range loop
         Scarti(I) := abs(V - Q*4.0/(Pi*(0.001*Mio_Float(DN(I)))**2));
      end loop;
      Piu_Vicina := Min(Scarti);
      return DN(Piu_Vicina.Indice);
   end DND;
   function Leggi_Dato(Dati : String; Nome : String; Predefinito : Mio_Float; Descrizione : String := " ") return Mio_Float is
      Numero_Dec : constant String := "(\s+[+-]?\d*\.\d*[eE]?[+-]?\d*)";
      Nome_Match : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile( Nome & Numero_Dec, GNAT.Regpat.Case_Insensitive);
      Risult : GNAT.Regpat.Match_Array(0..3);
      Dato : Mio_Float;
   begin
      GNAT.Regpat.Match(Nome_Match, Dati, Risult); 
      Dato := (if Risult(1).First > 0 and Risult(1).Last > 0 then Mio_Float'Value(Dati(Risult(1).First..Risult(1).Last)) else Predefinito);
      Put(Nome & " = " );
      Put(Dato, 3, 2, 0);
      Put_Line( " : " & Descrizione);
      return Dato;
   end Leggi_Dato;
   function Leggi_Dato(Dati : String; Nome : String; Predefinito : Positive; Descrizione : String := " ") return Positive is
      Numero_Pos : constant String := "(\s+\d*)";
      Nome_Match : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile( Nome & Numero_Pos, GNAT.Regpat.Case_Insensitive);
      Risult : GNAT.Regpat.Match_Array(0..3);
      Dato : Positive;
   begin
      GNAT.Regpat.Match(Nome_Match, Dati, Risult); 
      Dato := (if Risult(1).First > 0 and Risult(1).Last > 0 then Positive'Value(Dati(Risult(1).First..Risult(1).Last)) else Predefinito);
      Put_Line(Nome & " = " & Dato'Img & " : " & Descrizione);
      return Dato;
   end Leggi_Dato;
end LS.utili;

with LS.Utili; use LS.Utili;
with Ada.Text_IO; use Ada.Text_IO;
with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with LS.Integer_IO; use LS.Integer_IO;
with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;
with Ada.Numerics.Generic_Elementary_Functions;
with GNAT.Regpat;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Arrays;

generic
   Num_Equaz : Positive;
   type Reale is digits <>;
package LS.Calcoli is
   package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays(Real => Reale);
   use Real_Arrays;
   package Complex_Types is new Ada.Numerics.Generic_Complex_Types(Reale);
   use Complex_Types;
   package Complex_Arrays is new Ada.Numerics.Generic_Complex_Arrays(Real_Arrays, Complex_Types);
   use Complex_Arrays;
   subtype Matr_Compl is Complex_Arrays.Complex_Matrix(1..Num_Equaz, 1..Num_Equaz);
   subtype Vett_Compl is Complex_Arrays.Complex_Vector(1..Num_Equaz);
   package Reale_IO is new Ada.Text_IO.Float_IO(Reale);
   package Funzioni_Elementari is new Ada.Numerics.Generic_Elementary_Functions(Reale);
end LS.Calcoli;

with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with GNAT.Regpat;
with Ada.Calendar;
with Ada.Numerics.Float_Random; 
with LS.Calcoli;
with Ada.Command_Line;
procedure Prova_Complex_1 is
   Num_Eq : Positive;
   G : Ada.Numerics.Float_Random.Generator;
begin
   Num_Eq := Positive'Value(Ada.Command_Line.Argument(1));
   declare
      package Prova is new LS.Calcoli(Num_Eq, Mio_Float);
      use Prova.Complex_Types;
      use Prova.Complex_Arrays;
      A : Prova.Matr_Compl;
      B, C : Prova.Vett_Compl;
   begin
      Ada.Numerics.Float_Random.Reset(G,Integer(Ada.Calendar.Seconds(Ada.Calendar.Clock)));
      for i in 1..Num_Eq loop
         B(i) := Mio_Float(Ada.Numerics.Float_Random.Random(G)) + J*Mio_Float(Ada.Numerics.Float_Random.Random(G));
         for k in 1..Num_Eq loop
            A(i,k) := Mio_Float(Ada.Numerics.Float_Random.Random(G)) + J*Mio_Float(Ada.Numerics.Float_Random.Random(G));
         end loop;
      end loop;
      C := Solve(A, B);
      Put(Re(C(1)));
   end;
end Prova_Complex_1;

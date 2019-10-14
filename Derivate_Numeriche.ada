with Ada.Text_IO; use Ada.Text_IO;
with LS.Numerico; use LS.Numerico;
generic
   type Reale is digits <>;
   type Param is private;
package LS.Derivate_Numeriche is
   Con : Reale := 1.4;
   Con2 : Reale := Con**2;
   Big : Reale := 1.0e30;
   Ntab : Positive := 10;
   Safe : Reale := 2.0;
   A : array(1..Ntab, 1..Ntab) of Reale := (others => (others => 0.0 ));
   H_è_zero : exception;
   type Risultato is record
      Deriv, Errore : Reale;
   end record;
   type Accesso_A_Funzione is access function(X : Reale; Par : Param) return Reale;
   function Derivata(Func : Accesso_A_Funzione; X, H : Reale; Par : Param) return Risultato;
end LS.Derivate_Numeriche;
package body LS.Derivate_Numeriche is
   function Derivata(Func : Accesso_A_Funzione; X, H : Reale; Par : Param) return Risultato
   is
      HH, Fac, Err, Errt : Reale;
      Df_Err : Risultato;
      function max(A,B : Reale) return Reale is
      begin
         if A > B then
            return A;
         else
            return B;
         end if;
      end max;
   begin
      if H = 0.0 then raise H_è_zero; end if;
      HH := H;
      A(1,1) := (Func(X + HH, Par) - Func(X - HH, Par)) / (2.0*HH);
      Err := Big;
      for i in 2..NTAB loop
         hh := hh / Con;
         A(1,i) := (Func(X + HH, Par) - Func(X - HH, Par)) / (2.0*HH);
         Fac := Con2;
         for j in 2..I loop
            A(j,i) := (A(j-1,i)*Fac-A(j-1,i-1))/(Fac-1.0);
            Fac := Con2*Fac;
            Errt := max(abs(A(j,i)-A(j-1,i)),abs(A(j,i)-A(j-1,i-1)));
            if Errt <= Err then Err := Errt; end if;
            Df_Err := (A(j,i), Err);
         end loop;
         if abs(A(i,i)-A(i-1,i-1)) >= SAFE*Err then return Df_Err; end if;
      end loop;
      return Df_Err;
   end Derivata;
end LS.Derivate_Numeriche;

with Ada.Text_IO; use Ada.Text_IO;
with LS.Numerico; use LS.Numerico;
package LS.Prova is
   function Funz_Prova(X, Y : Mio_Float) return Mio_Float;
end LS.Prova;
package body LS.Prova is
   function Funz_Prova(X, Y : Mio_Float) return Mio_Float is begin
      return X**2 * Y;
   end Funz_Prova;
end LS.Prova;
with Ada.Text_IO; use Ada.Text_IO;
with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with LS.Prova; use LS.Prova;
with LS.Derivate_Numeriche;
procedure LS.Prova_Deriv is
   package D_N is new LS.Derivate_Numeriche(Reale => Mio_Float, Param => Mio_Float);
   use D_N;
   Ris : Risultato;
begin
   Ris := Derivata(Funz_Prova'access, 1.0, 5.0, 2.0);
   Put("D = "); Put(Ris.Deriv); Put("; Errore ="); Put(Ris.Errore);
end LS.Prova_Deriv;

with Ada.Numerics; use Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;
with LS.Float_IO; use LS.Float_IO;
with LS.Numerico; use LS.Numerico;

generic
   type Reale is digits <>;
   type Param is private;
package LS.Solve1D_Gen is
   type Stampa_Si_No is (Si, No);
   Devo_Stampare : Stampa_Si_No := Si;
   procedure Stampa; procedure Non_Stampare;
   function solve(func : access function(X : Reale; Par : Param) return Reale;
                  x_start : Reale; Par : Param) return Reale;
   function solve(func : access function(X : Reale; Par : Param) return Reale;
                  x1, x2 : Reale; Par : Param) return Reale;
   function solve2(func : access function(X : Reale; Par : Param) return Reale;
                   x1, x2 : Reale; Par : Param) return Reale
     renames Solve;
   function solve(func : access function(X : Reale; Par : Param) return Reale;
                  x_start : Reale; Par : Param; Da_Dove : String) return Reale;
   function solve(func : access function(X : Reale; Par : Param) return Reale;
                  x1, x2 : Reale; Par : Param; Da_Dove : string) return Reale;
   function solve2(func : access function(X : Reale; Par : Param) return Reale;
                   x1, x2 : Reale; Par : Param; Da_Dove : string) return Reale
     renames Solve;
   Radice_Non_Delimitata, Intervallo_Nullo: exception;
   private
   function min(a, b : Reale) return Reale;
   function signum(a : Reale) return Reale;
   procedure zbrac(func : access function(X : Reale; Par : Param) return Reale;
                   x1, x2 : in out Reale; succes : out Boolean; Par : Param);
   function zbrent(func : access function(X : Reale; Par : Param) return Reale;
                   x1, x2, tol : Reale; Par : Param) return Reale;
   procedure Plotta_Se_Non_Trova(Func : access function(X : Reale; Par : Param) return Reale;
                                 X1, X2 : Reale; Par : Param);
end LS.Solve1D_Gen;
package body LS.Solve1D_Gen is
   tol : Reale := 1.0e-6; -- usata da solve e solve2
   Factor : Reale := 1.6;
   Ntry : Positive := 50;  -- usate da zbrac
   ITMAX : Positive := 100;
   eps : Reale := 3.0e-8;  -- usate da zbrent
   function min(a, b : Reale) return Reale is
   begin
     if a >= b then
       return b;
     else
       return a;
     end if;
   end min;
   function signum(a : Reale) return Reale is
   begin
     if a > 0.0 then
       return 1.0;
     elsif a < 0.0 then
       return -1.0;
     else
       return 0.0;
     end if;
   end signum;
   function solve(func : access function(X : Reale; Par : Param) return Reale;
                  x_start : Reale; Par : Param; Da_Dove : String) return Reale is
      Ris : Reale;
   begin
      Ris := Solve(Func, X_Start, Par);
      return Ris;
   exception
      when Radice_Non_Delimitata|Intervallo_Nullo =>
         if Devo_Stampare = Si then
            Put("Chiamata da "&Da_Dove); New_Line;
            Put("x = "&Reale'Image(X_Start)&"; F(x) = "&Reale'Image((Func(X_Start,Par))));
            New_Line;
         end if;
         return Ris;
   end Solve;
   function solve(func : access function(X : Reale; Par : Param) return Reale;
                  x_start : Reale; Par : Param) return Reale is
     x1 : Reale := x_start;
     x2 : Reale := x_start*1.1;
     succes : Boolean;
     tolleranza : Reale;
     begin
        zbrac(func, x1, x2, Succes, Par);
        if Succes then
           tolleranza := tol*(x1+x2)/2.0;
           return zbrent(func, x1, x2, Tolleranza, Par);
        else
           if Devo_Stampare = Si then
              Put_Line("You have to guess an initial range in zbrac");
              Plotta_Se_Non_Trova(Func, X1, X2, Par);
           end if;
           raise Radice_Non_Delimitata;
        end if;
     end solve;
  function solve(func : access function(X : Reale; Par : Param) return Reale;
                    X1, X2 : Reale; Par : Param; Da_Dove : String) return Reale is
      Ris : Reale;
   begin
      Ris := Solve(Func, X1, X2, Par);
      return Ris;
   exception
      when Radice_Non_Delimitata|Intervallo_Nullo =>
         Plotta_Se_Non_Trova(Func, X1, X2, par);
         if Devo_Stampare = Si then
            Put("Chiamata da "&Da_Dove); New_Line;
            Put("x1 = "&Reale'Image(X1)&"; F(x1) = "&Reale'Image(Func(X1,Par)) );New_Line;
            Put("x2 = "&Reale'Image(X2)&"; F(x2) = "&Reale'Image(Func(X2,Par)) );New_Line;
         end if;
         return Ris;
  end Solve;
  function solve(func : access function(X : Reale; Par : Param) return Reale;
                     x1, x2 : Reale; Par : Param) return Reale is
     tolleranza : Reale;
     begin
       tolleranza := tol*(x1+x2)/2.0;
       return zbrent(func, x1, x2, Tolleranza, Par);
     end solve;
   procedure zbrac(func : access function(X : Reale; Par : Param) return Reale;
                     x1, x2 : in out Reale; succes : out Boolean; Par : Param) is
   -- Delimita uno zero di una funzione, che poi sara' calcolato da zbrent
   -- Copr. 1986-92 Numerical Recipes Software m2D[!.
   f1, f2 : Reale;
   begin
     if x1 = x2 then
           if Devo_Stampare = Si then
              Put_Line("You have to guess an initial range in zbrac");
           end if;
        raise Intervallo_Nullo;
     end if;
     f1 := func(x1, Par); f2 := func(x2, Par);
     succes := true;
     for j in 1..NTRY loop
       if f1*f2 < 0.0 then
         return;
       end if;
       if abs(f1) < abs(f2) then
         x1 := x1 + FACTOR*(x1-x2);
         f1 := func(x1, Par);
       else
         x2 := x2 + FACTOR*(x2-x1);
         f2 := func(x2, Par);
       end if;
     end loop;
     succes := false;
   end zbrac;
   function zbrent(func : access function(X : Reale; Par : Param) return Reale;
                   x1, x2, tol : Reale; Par : Param) return Reale is
   -- Calcola la radice di una funzione col metodo di van Wijngaarden, Dekker, Brent
   -- Copr. 1986-92 Numerical Recipes Software m2D[!.
   a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm : Reale;
   begin
     a := x1; b := x2;
     fa := func(a, Par); fb := func(b, Par);
     if ((fa > 0.0 and fb > 0.0) or (fa < 0.0 and fb < 0.0))
       and Devo_Stampare = Si then
        Put_Line("Root must be bracketed for zbrent");
        Plotta_Se_Non_Trova(Func, X1, X2, par);
        raise Radice_Non_Delimitata;
     end if;
     c := b; fc := fb;
     for iter in 1..ITMAX loop
         if (fb>0.0 and fc>0.0) or (fb<0.0 and fc<0.0) then
            c := a; fc := fa;
            d := b - a; e := d;
         end if;
         if abs(fc) < abs(fb) then
            a := b; fa := fb;
            b := c; fb := fc;
            c := a; fc := fa;
         end if;
         tol1 := 2.0*EPS*abs(b) + 0.5*tol;
         xm := 0.5*(c-b);
         if (abs(xm) <= tol1) or (fb = 0.0) then
            return b;
         end if;
         if abs(e) >= tol1 and abs(fa) > abs(fb) then
            s := fb / fa;
            if a = c then
               p := 2.0*xm*s;
               q := 1.0 - s;
            else
               q := fa/fc;
               r := fb/fc;
               p := s*( 2.0*xm*q*(q-r) - (b-a)*(r-1.0) );
               q := (q-1.0)*(r-1.0)*(s-1.0);
            end if;
            if p > 0.0 then
               q := -q;
            end if;
            p := abs(p);
            if 2.0*p < min( 3.0*xm*q-abs(tol1*q), abs(e*q) ) then
               e := d;
               d := p/q;
            else
               d := xm; e := d;
            end if;
         else
            d := xm; e := d;
         end if;
         a := b; fa := fb;
         if abs(d) > tol1 then
            b := b + d;
         else
            b := b + tol1*signum(xm);
         end if;
         fb := func(b, Par);
     end loop;
     if Devo_Stampare = Si then
        Put_Line("zbrent exceeding maximum iterations");
     end if;
     return b;
   end zbrent;
   procedure Stampa is begin Devo_Stampare := Si; end Stampa;
   procedure Non_Stampare is begin Devo_Stampare := No; end Non_Stampare;
   procedure Plotta_Se_Non_Trova(Func : access function(X : Reale; Par : Param) return Reale;
                                 X1, X2 : Reale; Par : Param) is
      Num_Punti : Positive := 100;
      Dx : Reale := (X2 - X1)/Reale(Num_Punti - 1);
      X_Ini : Reale := X1 - Dx;
      Nuovo_File : File_Type;
      Nome_File : String := "NienteRadice";
   begin
      Create(Nuovo_File,Out_File,Nome_File);
      Set_Output(Nuovo_File);
      Put("set term postscript color"); New_Line;
      Put("set output '"); Put(Nome_File); Put(".ps'"); New_Line;
      Put("plot'-' title '"); Put(Nome_File); Put("' with lines"); New_Line;
      for i in 1..Num_Punti loop
         Put(Mio_Float(X_Ini)); Put("   "); Put(Mio_Float(Func(X_Ini, Par))); New_Line;
         X_Ini := X_Ini + Dx;
      end loop;
      Close(Nuovo_File);
      Set_Output(Standard_Output);
   end Plotta_Se_Non_Trova;
end LS.Solve1D_Gen;

with LS.Numerico; use LS.Numerico;
package LS.Prova is
   function da_annullare(x : Mio_Float; P : Mio_Float) return Mio_Float;
end LS.Prova;
package body LS.Prova is
  function da_annullare(x : Mio_Float; P : Mio_Float) return Mio_Float is
  begin
    return x**2 - P;
  end da_annullare;
end LS.Prova;

with Ada.Text_IO; use Ada.Text_IO;
with LS.Numerico; use LS.Numerico;
with LS.Solve1D_Gen;
with LS.Prova; use LS.Prova;
procedure LS.Prova_solve is
   package Nuovo_Solve is new LS.Solve1D_Gen(Reale => Mio_Float, Param => Mio_Float );
   use Nuovo_Solve;
   ris : Mio_Float;
begin
   ris := solve(da_annullare'Access, 1.0, 9.0);
   Put_Line("Risultato = " & Mio_Float'Image(ris));
   ris := solve(da_annullare'Access, 1.0, 5.0, 9.0);
   Put_Line("Risultato = " & Mio_Float'Image(ris));
   ris := solve2(da_annullare'Access, 1.0, 2.0, 9.0);
   Put_Line("Risultato = " & Mio_Float'Image(ris));
end LS.Prova_solve;

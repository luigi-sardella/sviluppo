with Ada.Text_IO; use Ada.Text_IO;
with LS.Float_IO; use LS.Float_IO;
with LS.Numerico; use LS.Numerico;
package LS.solve1D is
  type Stampa_Si_No is (Si, No);
  Devo_Stampare : Stampa_Si_No := Si;
  function solve(func : access function(x : Mio_Float) return Mio_Float;
                 x_start : Mio_Float) return Mio_Float;
  function solve2(func : access function(x : Mio_Float) return Mio_Float;
                 x1, x2 : Mio_Float) return Mio_Float;
--function solve2(func : access function(x : Mio_Float) return Mio_Float;
--               x1, x2 : Mio_Float) return Mio_Float renames Solve;
  function solve(func : access function(X : Mio_Float) return Mio_Float;
                 x_start : Mio_Float; Da_Dove : String) return Mio_Float;
  function solve2(func : access function(X : Mio_Float) return Mio_Float;
                 x1, x2 : Mio_Float; Da_Dove : string) return Mio_Float;
--function solve2(func : access function(X : Mio_Float) return Mio_Float;
--               x1, x2 : Mio_Float; Da_Dove : string) return Mio_Float
--  renames Solve;
  Radice_Non_Delimitata, Intervallo_Nullo: exception;
  private
  function min(a, b : Mio_Float) return Mio_Float;
  function signum(a : Mio_Float) return Mio_Float;
  procedure zbrac(func : access function(x : Mio_Float) return Mio_Float;
                  x1, x2 : in out Mio_Float; succes : out Boolean);
  function zbrent(func : access function(x : Mio_Float) return Mio_Float;
                  x1, x2, tol : Mio_Float) return Mio_Float;
  procedure Plotta_Se_Non_Trova(Func : access function(X : Mio_Float) return Mio_Float;
                                X1, X2 : Mio_Float);
end LS.solve1D;
package body LS.solve1D is
  tol : Mio_Float := 1.0e-6; -- usata da solve e solve2
  Factor : Mio_Float := 1.6;
  Ntry : Positive := 50;  -- usate da zbrac
  ITMAX : Positive := 100;
  eps : Mio_Float := 3.0e-8;  -- usate da zbrent
  function min(a, b : Mio_Float) return Mio_Float is
  begin
    if a >= b then
      return b;
    else
      return a;
    end if;
  end min;
  function signum(a : Mio_Float) return Mio_Float is
  begin
    if a > 0.0 then
      return 1.0;
    elsif a < 0.0 then
      return -1.0;
    else
      return 0.0;
    end if;
  end signum;
  function solve(func : access function(X : Mio_Float) return Mio_Float;
                  x_start : Mio_Float; Da_Dove : String) return Mio_Float is
       Ris : Mio_Float;
    begin
       Ris := Solve(Func, X_Start);
       return Ris;
    exception
       when Radice_Non_Delimitata|Intervallo_Nullo =>
          if Devo_Stampare = Si then
             Put("Chiamata da "&Da_Dove); New_Line;
             Put("x = "&Mio_Float'Image(X_Start)&"; F(x) = "
                 &Mio_Float'Image((Func(X_Start))));
             New_Line;
          end if;
          return Ris;
   end Solve;
   function solve(func : access function(x : Mio_Float) return Mio_Float;
                 x_start : Mio_Float) return Mio_Float is
     x1 : Mio_Float := x_start;
     x2 : Mio_Float := x_start*1.1;
     succes : Boolean;
     tolleranza : Mio_Float;
     begin
        zbrac(func, x1, x2, succes);
        if Succes then
           tolleranza := tol*(x1+x2)/2.0;
           return zbrent(func, x1, x2, tolleranza);
        else
           Put_Line("You have to guess an initial range in zbrac");
	   Plotta_Se_Non_Trova(Func, X1, X2);
           raise Radice_Non_Delimitata;
        end if;
     end solve;
     function solve2(func : access function(X : Mio_Float) return Mio_Float;
                    X1, X2 : Mio_Float; Da_Dove : String) return Mio_Float is
         Ris : Mio_Float;
      begin
         Ris := Solve2(Func, X1, X2);
         return Ris;
      exception
         when Radice_Non_Delimitata|Intervallo_Nullo =>
            Plotta_Se_Non_Trova(Func, X1, X2);
            if Devo_Stampare = Si then
               Put("Chiamata da "&Da_Dove); New_Line;
               Put("x1 = "&Mio_Float'Image(X1)&"; F(x1) = "&Mio_Float'Image(Func(X1)));
               New_Line;
               Put("x2 = "&Mio_Float'Image(X2)&"; F(x2) = "&Mio_Float'Image(Func(X2)));
               New_Line;
            end if;
	    return Ris;
      end Solve2;
     function solve2(func : access function(x : Mio_Float) return Mio_Float;
                   x1, x2 : Mio_Float) return Mio_Float is
     tolleranza : Mio_Float := tol*(x1+x2)/2.0;
     begin
       return zbrent(func, x1, x2, tolleranza);
     end solve2;
   procedure zbrac(func : access function(x : Mio_Float) return Mio_Float;
                    x1, x2 : in out Mio_Float; succes : out Boolean) is
   -- Delimita uno zero di una funzione, che poi sara' calcolato da zbrent
   -- Copr. 1986-92 Numerical Recipes Software m2D[!.
     f1, f2 : Mio_Float;
   begin
     if x1 = x2 then
        Put_Line("You have to guess an initial range in zbrac");
        raise Intervallo_Nullo;
     end if;
     f1 := func(x1); f2 := func(x2);
     succes := true;
     for j in 1..NTRY loop
       if f1*f2 < 0.0 then
         return;
       end if;
       if abs(f1) < abs(f2) then
         x1 := x1 + FACTOR*(x1-x2);
         f1 := func(x1);
       else
         x2 := x2 + FACTOR*(x2-x1);
         f2 := func(x2);
       end if;
     end loop;
     succes := false;
   end zbrac;
   function zbrent(func : access function(x : Mio_Float) return Mio_Float;
                  x1, x2, tol : Mio_Float) return Mio_Float is
   -- Calcola la radice di una funzione col metodo di van Wijngaarden, Dekker, Brent
   -- Copr. 1986-92 Numerical Recipes Software m2D[!.
   a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm : Mio_Float;
   begin
     a := x1; b := x2;
     fa := func(a); fb := func(b);
     if (fa > 0.0 and fb > 0.0) or (fa < 0.0 and fb < 0.0) then
        Put_Line("Root must be bracketed for zbrent");
	Plotta_Se_Non_Trova(Func, X1, X2);
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
         fb := func(b);
     end loop;
     Put_Line("zbrent exceeding maximum iterations");
     return b;
   end zbrent;
   procedure Plotta_Se_Non_Trova(Func : access function(X : Mio_Float) return Mio_Float;
                                 X1, X2 : Mio_Float) is
      Num_Punti : Positive := 100;
      Dx : Mio_Float := (X2 - X1)/Mio_Float(Num_Punti - 1);
      X_Ini : Mio_Float := X1 - Dx;
      Nuovo_File : File_Type;
      Nome_File : String := "NienteRadice";
   begin
      Create(Nuovo_File,Out_File,Nome_File);
      Set_Output(Nuovo_File);
      Put("set term postscript color"); New_Line;
      Put("set output '"); Put(Nome_File); Put(".ps'"); New_Line;
      Put("plot'-' title '"); Put(Nome_File); Put("' with lines"); New_Line;
      for i in 1..Num_Punti loop
         Put(X_Ini); Put("   "); Put(Func(X_Ini)); New_Line;
	 X_Ini := X_Ini + Dx;
      end loop;
      Close(Nuovo_File);
      Set_Output(Standard_Output);
   end Plotta_Se_Non_Trova;
end LS.solve1D;

with LS.Numerico; use LS.Numerico;
package LS.Prova is
   function da_annullare(x : Mio_Float) return Mio_Float;
end LS.Prova;
package body LS.Prova is
  function da_annullare(x : Mio_Float) return Mio_Float is
  begin
    return x**2 - 9.0;
  end da_annullare;
end LS.Prova;

with Ada.Text_IO; use Ada.Text_IO;
with LS.Numerico; use LS.Numerico;
with LS.solve1D; use LS.solve1D;
with LS.Prova; use LS.Prova;
procedure LS.Prova_solve is
  ris : Mio_Float;
begin
  ris := solve(Da_Annullare'access, 1.0);
  Put_Line("Risultato = " & Mio_Float'Image(ris));
end LS.Prova_solve;

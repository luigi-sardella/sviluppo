-- operazioni con vettori e matrici secondo Ada 2005
with Ada.Text_IO; use Ada.Text_IO;
with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with LS.Integer_IO; use LS.Integer_IO;
with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Elementary_Functions;

generic
   Num_Equaz : Positive;
   type Reale is digits <>;
   type Param is private;
package LS.Newton is
   package Vet_Mat is new Ada.Numerics.Generic_Real_Arrays(Real => Reale); use Vet_Mat;
   subtype Vett is Vet_Mat.Real_Vector(1..Num_Equaz);
   subtype Matr is Vet_Mat.Real_Matrix(1..Num_Equaz, 1..Num_Equaz);
   package Reale_IO is new Ada.Text_IO.Float_IO(Reale);
   package Funzioni_Elementari is new Ada.Numerics.Generic_Elementary_Functions(Reale);
   Gvet,Pvet,Xvet,X_Old,Fvet : Vett := (others => 0.0);
   Matr_Jac : Matr := (others => (others => 0.0 ));
   type Eqni is access function (Xvet : Vett; Par : Param) return Vett;
   type Check is (Normal, Local);
   Newt_Check, Lnsr_Check : Check := Normal;
   Max_Its : constant Mio_Intero := 10000;
   Tol_F, Tol_Min : constant Reale := 1.0E-06;
   Tol_X : constant Reale := 1.0E-05;
   Alf, Eps : constant Reale := 1.0E-04;
   Stp_Mx : constant Reale := 100.0;
   F_New, F_Old, Stp_Max : Reale;
-- procedure Val_Ini(Y_Ini : Vett);
-- procedure Cambia_Parametri (Maxstp : Mio_Intero := 100_000; Eps : Reale := 1.0E-04;
--                             H1 : Reale := 0.1; Hmin : Reale := 0.0; Tiny : Reale := 1.0E-30);
   function F_Min(Funz : Eqni; Xvet : Vett; Par : Param) return Reale; 
   procedure Fd_Jac(Funz : Eqni; Par : Param ); 
   procedure Ln_Srch(Funz : Eqni; Par : Param); 
-- procedure Newt(Funz : Eqni; N_Eq : Positive := Num_Equaz; Par : Param); 
   procedure Newt(Funz : Eqni; Par : Param); 
   function Max(V : Vett) return Reale;
   function Max(A, B : Reale) return Reale;
   procedure Put(V : in Vett);
   Roundoff_Problem, MaxIts_Exceeded_Newt : exception;
end LS.Newton;

package body LS.Newton is

   procedure Newt(Funz : Eqni; Par : Param) is 
-- Given an initial guess X(n) for a root in n dimensions, finds the root
-- by Newton's method. The vector of functions to be zeroed, called Fvet(n)
-- in the routine below, is returned by a user-supplied subroutine. The output
-- quantity 'check' is 0 on a normal return and 1 if the routine has converged
-- to a local minimum of the function F_Min. In this case try restarting
-- from a different initial guess.
-- Parameters: Max_Its is the maximum number of iterations; Tol_F sets the
-- convergence criterion on function values; Tol_Min sets the criterion for
-- deciding whether spurious convergence to a minimum of F_Min has occurred; Tol_X
-- is the convergence criterion on dx; Stp_Mx is the scaled maximum step length
-- allowed in line searches.
      Fnum,Test : Reale;
      Iter : Positive;
      N_Eq : Positive := Xvet'Length;
   begin
      Fnum := F_Min(Funz, Xvet, Par);-- The vector Fvet is also computed by this call 
      Test := Max(abs(Fvet));        -- Test for initial guess being a root.
                                     -- Use more stringent test than simply Tol_F
      Stp_Max := Stp_Mx * Max(abs(Xvet),Reale(N_Eq)); -- Calculate stmMax for line searches
      for its in 1..Max_Its loop     -- Start of iteration loop
         Iter := Positive(Its);
         exit when Test < 0.01*Tol_F;
         Fd_Jac( Funz, Par ); 
	 Gvet := Matr_Jac*Fvet;
         X_Old := Xvet;
         F_Old := Fnum;
	 Pvet := -Solve(Matr_Jac, Fvet);
         Xvet := Pvet;
         Ln_Srch( Funz, Par );       -- Returns new X, it also calculates Fvet at new X when it calls F_Min 
         Test := Max(abs(Fvet));     -- Test of convergence on function values
         if Test < Tol_F then
            Newt_Check := Normal;
            exit;
         end if;
         if Newt_Check = Local then  -- Check for zero gradient, i.e. spurious convergence
            Test := Max( abs(Gvet) * Max(abs(Xvet), 1.0) / Max(Fnum,0.5*Reale(N_Eq)) );
            if Test < Tol_Min then
               Newt_Check := Local;
            else
               Newt_Check := Normal;
            end if;
            exit;
         end if;
         Test := Max( abs(Xvet - X_Old) / Max(abs(Xvet))); -- , 1.0)); -- Test for convergence on dx
	 --Put(Test,3,3,1);Put("  ");
         exit when Test < Tol_X;
      end loop;     
      if Iter >= Positive(Max_Its) then raise MaxIts_Exceeded_Newt; end if;
   end Newt;

   procedure Ln_Srch(Funz : Eqni; Par : Param) is 
-- Given a n-dimendional point X_Old(n), the value of the function and gradient
-- there, F_Old and G(n), finds a  new point X(n) along the direction P from
-- X_Old where the function F_Min has decreased 'sufficiently'. The new function
-- value is returned in f. 'stpmax' is an output quantity that limits the strength
-- of the steps so that you do not try to evaluate the function in regions where
-- it isundefined or subject to underflow. P is usually the Newton direction.
-- The output quantity 'check' is 0 on a normal exit. It is 1 when X  is too close
-- to X_Old. In a minimization algorithm, this usually signals convergence and can
-- be ignored. However, in a zero-finding algorithm the calling program should check
-- whether the convergence is spurious.
-- Parameters: ALF ensures sufficient decrease in function value. Tol_X is the
-- convergence criterion on dx.
      Mod_P,Slope,Test,Lamb_In,Lamb,Tmplam,Rhs1,Rhs2,A,B,Discr : Reale;
      Lamb2,F_New2,F_Old2 : Reale := 0.0; -- così il compilatore non si lagna più; sono definite e usate dopo la prima iterazione
   begin
      Lnsr_Check := Normal;
      Mod_P := abs(Pvet);   -- Scale if attempted step is too big
      if Mod_P > Stp_Max then Pvet := Pvet * Stp_Max / Mod_P; end if;
      Slope := Gvet*Pvet; -- prodotto scalare
      Test := Max( abs(Pvet) / Max(abs(X_Old),1.0) ); -- Compute lambda min.
      Lamb_In := Tol_X / Test;
      Lamb := 1.0;             -- Always try full Newton step first
      loop                     -- Start of iteration loop
        Xvet := X_Old + Lamb * Pvet;
        F_new := F_Min(Funz, Xvet, Par); 
        if Lamb < Lamb_In then -- Convergence on dx. For zero finding, the calling program
          Xvet := X_Old;       -- should verify the convergence
          Lnsr_Check := Local;
          exit;
        elsif F_new <= F_Old + ALF * Lamb * slope then
          exit;                -- Sufficient function decrease
        else                   -- Backtrack
          if Lamb = 1.0 then   -- First  time
            tmplam := - Slope / (2.0*(F_New-F_Old-Slope));
          else                 -- Subsequent backtracks
            rhs1 := F_new-F_Old-Lamb*slope;
            rhs2 := F_new2-F_Old2-Lamb2*slope;
            a := (rhs1/Lamb**2-rhs2/Lamb2**2)/(Lamb-Lamb2);
            b := (-Lamb2*rhs1/Lamb**2+Lamb*rhs2/Lamb2**2)/(Lamb-Lamb2);
            if a = 0.0 then
              tmplam := -slope/(2.0*b);
            else
              Discr := b*b-3.0*a*slope;
              if Discr < 0.0 then raise Roundoff_Problem; end if;
            end if;
           tmplam := (-B + Funzioni_Elementari.Sqrt(Discr))/(3.0*a);
            if tmplam > 0.5*Lamb then tmplam := 0.5 * Lamb; end if; -- lambda <= 0.5 lambda1
         end if;
        end if;
        Lamb2 := Lamb;
        F_new2 := F_new;
        F_Old2 := F_Old;
        Lamb := max(tmplam,0.1*Lamb); -- lambda >= 0.1 lambda1. Try again
      end loop;
   end Ln_Srch;

   function F_Min(Funz : Eqni; Xvet : Vett; Par : Param) return Reale is 
   begin
      Fvet := Funz(Xvet, Par); 
      return 0.5 * Fvet*Fvet; -- prodotto scalare
   end F_Min;

   procedure Fd_Jac(Funz : Eqni; Par : Param ) is 
--  Computes forward-difference approssimation to Jacobian. On input, X(n) is
--  the point at which the Jacobian is to be evaluated, Fvet(n) is  the vector
--  of function values at the point, routine Funcv returns the vector of function
--  values at X.
--  Parameter EPS is the approssimate square root of the machine precision.
      Temp,H : Reale;
      F : Vett;
   begin
      for J in Xvet'range loop
         Temp := Xvet(j);
         H := Eps * abs(Temp);
         if H = 0.0 then H := Eps; end if;
         Xvet(j) := Temp + H; -- Trick to reduce finite precision error
         H := Xvet(j) - Temp;
         F := Funz(Xvet, Par); 
         Xvet(J) := Temp;
         for i in Xvet'range loop
            Matr_Jac(i,j) := (F(i) - Fvet(i)) / H; -- Forward difference formula
         end loop;
      end loop;
   end Fd_Jac;
   
   function Max(V : Vett) return Reale is
      M : Reale := V(V'First);
      M1 : Reale;
   begin
      for i in V'First+1..V'Last loop
         M1 := V(i);
         if M1 > M then
            M := M1;
         end if;
      end  loop;
      return M;
   end Max;
   
   function Max(A,B : Reale) return Reale is
   begin
      if A > B then
         return A;
      else
         return B;
      end if;
   end Max;
   
   procedure Put(V : in Vett) is
   begin
      for I in V'Range loop
         Reale_IO.Put(V(I)); Put(" ");
      end loop;
      New_Line;
   end Put;
   
   --  (C) Copr. 1986-92 Numerical Recipes Software m2D[!.
end LS.Newton;

with Ada.Text_IO; use Ada.Text_IO;
with LS.Numerico; use LS.Numerico;
with LS.Newton;
procedure LS.Prova_Newton is
   package N_Nr is new LS.Newton(Num_Equaz => 4, Reale => Mio_Float, Param => Mio_Float);
   Par : Mio_Float := 2.0;
   function Fprova(X : N_NR.Vett; Par : Mio_float) return N_NR.Vett is
      Y : N_NR.Vett := (others => 0.0);
   begin
      Y(1) := X(1)**2 + X(2)**2 - Par;
      Y(2) := N_NR.Funzioni_Elementari.Exp(X(1) - Par/2.0) + X(4)**3 - Par;
      Y(3) := N_NR.Funzioni_Elementari.Exp(X(2) - Par) + X(3)**3 - Par/2.0;
      Y(4) := X(4)**5 - N_NR.Funzioni_Elementari.Sin(X(1));
      return Y;
   end Fprova;
begin
   N_NR.Xvet(1) := 2.0;
   N_NR.Xvet(2) := 0.5;
   N_NR.Xvet(3) := 1.5;
   N_NR.Xvet(4) := 0.3;
   New_Line; New_Line;
   N_NR.Newt(Fprova'Access, Par ); 
   N_NR.Put(N_NR.Xvet); New_Line; N_NR.Put(Fprova(N_NR.Xvet,Par));
end LS.Prova_Newton;

with Ada.Text_IO; use Ada.Text_IO;
with LS.Numerico; use LS.Numerico;
with LS.Newton;
procedure LS.Prova_Newton2 is
   package N_Nr is new LS.Newton(Num_Equaz => 4, Reale => Mio_Double, Param => Mio_Double);
   Par : Mio_Double := 2.0;
   function Fprova(X : N_NR.Vett; Par : Mio_Double) return N_NR.Vett is
      Y : N_NR.Vett := (others => 0.0);
   begin
      Y(1) := X(1)**2 + X(2)**2 - Par;
      Y(2) := N_NR.Funzioni_Elementari.Exp(X(1) - Par/2.0) + X(4)**3 - Par;
      Y(3) := N_NR.Funzioni_Elementari.Exp(X(2) - Par) + X(3)**3 - Par/2.0;
      Y(4) := X(4)**5 - N_NR.Funzioni_Elementari.Sin(X(1));
      return Y;
   end Fprova;
begin
   N_NR.Xvet(1) := 2.0;
   N_NR.Xvet(2) := 0.5;
   N_NR.Xvet(3) := 1.5;
   N_NR.Xvet(4) := 0.3;
   New_Line; New_Line;
   N_NR.Newt(Fprova'Access, Par ); 
   N_NR.Put(N_NR.Xvet); New_Line; N_NR.Put(Fprova(N_NR.Xvet,Par));
end LS.Prova_Newton2;


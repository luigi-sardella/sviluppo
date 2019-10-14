with Ada.Numerics;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Text_IO;
with LS.Numerico;
with LS.Float_IO;
with Ada.Integer_Text_IO;
with LS.Funzioni_Elementari;
with Ada.Calendar;
with LS.utili;
--with LS.LU;
generic
   N_X : Positive;
   N_Y : Positive;
   N_U : Natural;
   type Parametri_Ingresso is private;
package LS.ODE is
   use Ada.Text_IO;
   use LS.Numerico;
   use LS.Float_IO;
   use Ada.Integer_Text_IO;
   use LS.Funzioni_Elementari;
   use Ada.Calendar;
   use LS.utili;
-- use Risposte_IO;
-- package Lu_Y is new LS.LU(N_Max => N_Y); use Lu_Y;
   package Vet_Mat is new Ada.Numerics.Generic_Real_Arrays(Real => Mio_Float); -- use Vet_Mat;
   subtype Vett is Vet_Mat.Real_Vector(1..N_Y);
   subtype Matr is Vet_Mat.Real_Matrix(1..N_Y, 1..N_Y);
   type V_X is new Vettore(1..N_X);
   type V_Y is new Vettore(1..N_Y);
   type V_U is new Vettore(1..N_U);
   type M_Y is new Matrice(1..N_Y,1..N_Y);
   type DY_E_U is record
      DY : V_Y;
      U  : V_U;
   end record;
   type Da_Jacobi is record
      M : M_Y;
      V : V_Y;
   end record;
   type Risultati is record
      Asc : V_X;
      Ord : V_X;
   end record;
   type Derivate is access function(T : Mio_Float; Y : V_Y; Par : Parametri_Ingresso) return DY_E_U;
   type Rapporto is record
      Nok : Natural;
      Nbad : Natural;
      Nrhs : Natural;
      Kount : Natural;
   end record;
--   type Risposte is (S, X, L);
--   package Risposte_IO is new Enumeration_IO(Enum => Risposte); use Risposte_IO;
   type Si_O_No is (S, N);
   package Si_O_No_IO is new Enumeration_IO(Enum => Si_O_No); use Si_O_No_IO;
   type Normale_O_Stiff is (S, N);
   package N_O_S_IO is new Enumeration_IO(Enum => Normale_O_Stiff); use N_O_S_IO;
   type Gnu_Plot is (Gnuplot, Nognuplot);
--   procedure Opzioni(Risp : out Risposte; NOS : out Normale_O_Stiff; SN : out Si_O_No);
--   procedure File_IO(Az : P_Azione; Risp : Risposte; File_Dati : String);
   procedure Val_Ini(Y_Ini : V_Y);
   procedure Ode_Int(Deriv : Derivate; NOS : Normale_O_Stiff; Par : Parametri_Ingresso);
   procedure Cambia_Parametri (Maxstp : Mio_Intero := 100_000_000; Eps : Mio_Float := 1.0E-04;
                               H1 : Mio_Float := 0.1; Hmin : Mio_Float := 0.0; Tiny : Mio_Float := 1.0E-30);
   procedure Asse_X (X_Iniz,X_Fine,Dxsav : Mio_Float;
                     X_Inizio_Stampe,X_Fine_Stampe : Mio_Float:=0.0);
   function X_P(N : Positive) return Mio_Float;
   function Y_P(N : Positive) return V_Y;
   function U_P(N : Positive) return V_U;
   function Ris_Dx return Risultati;
   function Ris_Y(K : Positive) return Risultati;
   function Ris_U(K : Positive) return Risultati;
   procedure Plot_Curva(Curva : Risultati; Scala : Mio_Float; Nome_file : String := ""; Gnplt : Gnu_Plot := Gnuplot);
   procedure Plot_Curva(Curva : Risultati; Nome_file : String := ""; Gnplt : Gnu_Plot := Gnuplot);
   procedure Scrivi_Rapporto(Nome_File : String := "");
   procedure Put(V : V_Y);
   -- Eccezioni
   Stepsize : exception;
private
   procedure Rkck(X,H : in Mio_Float; Y, Dydx : in V_Y; Ytemp, Yerr : out V_Y; Par : Parametri_Ingresso);
   procedure Rkqs(Htry : in Mio_Float; Dydx : in V_Y; X : in out Mio_Float; Y : in out V_Y; Par : Parametri_Ingresso);
   procedure Stiff(Htry : in Mio_Float; Dydx : in V_Y; X : in out Mio_Float; Y : in out V_Y; Par : Parametri_Ingresso);
   function Jacobi_Dfdy(IC : Integer; X : Mio_Float; Y : V_Y; Par : Parametri_Ingresso) return V_Y;
   function Jacobi_Dfdx(X : Mio_Float; Y : V_Y; Par : Parametri_Ingresso) return V_Y;
   function Jacobi(X : Mio_Float; Y : V_Y; Par : Parametri_Ingresso) return Da_Jacobi;
   procedure Plot_Ris(R : Risultati; Scala : Mio_Float);
   Tempo : Time;
end LS.ODE;
package body LS.ODE is
   -- Parametri d'integrazione
   Maxstp_I : Positive := 100_000_000;
   Eps_I : Mio_Float := 1.0E-6;
   H1_I : Mio_Float := 0.1;
   Hmin_I : Mio_Float := 0.0;
   Tiny_I : Mio_Float := 1.0e-30;
   -- Valori iniziali e finali, anche delle stampe
   X_Ini_P, X_Fin_P : Mio_Float := 0.0; -- le stampe
   X_Iniz_I,X_Fine_I,Dxsav_I : Mio_Float;
   -- Dimensioni
   Xp, DXp : V_X :=(others => 0.0);
   type Mat_Yp is array(Xp'Range) of V_Y;
   type Mat_Up is array(Xp'Range) of V_U;
   Ystart, Yscal : V_Y :=(others => 0.0);
   Yp : Mat_Yp;
   Up : Mat_Up;
   -- Altro
   Nrhs : Natural;
   Hnext, Hdid : Mio_Float;
   RHS : Derivate;
   Rapp : Rapporto;
   procedure Asse_X (X_Iniz,X_Fine,Dxsav : Mio_Float;
                     X_Inizio_Stampe,X_Fine_Stampe : Mio_Float:=0.0) is
   begin
      X_Iniz_I := X_Iniz;
      X_Fine_I := X_Fine;
      Dxsav_I := Dxsav;
      if X_Inizio_Stampe = 0.0 then
         X_Ini_P := X_Iniz;
      else
         X_Ini_P := X_Inizio_Stampe;
      end if;
      if X_Fine_Stampe = 0.0 then
         X_Fin_P := X_Fine;
      else
         X_Fin_P := X_Fine_Stampe;
      end if;
   end Asse_X;
   procedure Cambia_Parametri (Maxstp : Mio_Intero := 100_000_000; Eps : Mio_Float := 1.0E-04;
              H1 : Mio_Float := 0.1; Hmin : Mio_Float := 0.0; Tiny : Mio_Float := 1.0E-30)
   is
   begin
      Eps_I := Eps;
      H1_I := H1;
      Hmin_I := Hmin;
      Tiny_I := Tiny;
      Maxstp_I := Positive(Maxstp);
   end Cambia_Parametri;
   procedure Val_Ini(Y_Ini : V_Y) is
   begin
      Ystart := Y_Ini;
   end Val_Ini;
   procedure Ode_Int(Deriv : Derivate; NOS : Normale_O_Stiff; Par : Parametri_Ingresso) is
      X : Mio_Float := X_Iniz_I;
      Nok,Nbad,Kount : Natural := 0;
      H, Xsav : Mio_Float;
      N_Xout : Natural;
      Nstp : Natural := 0;
      Der : DY_E_U;
      Y : V_Y := Ystart;
      Dydx : V_Y;
   begin
      Tempo := Clock;
      Nrhs := 0;
      RHS := Deriv;
      if X_Fine_I > X_Iniz_I then
        H := H1_I;
      else
        H := -H1_I;
      end if;
      N_Xout := Xp'Length;
      if N_Xout > 0 then Xsav := X - 2.0 * Dxsav_I; end if;
      loop
        Nstp := Nstp + 1;
        Der := RHS(X, Y, Par); Nrhs := Nrhs + 1;
        Dydx := Der.DY;
        case NOS is
           when N => Yscal := abs(Y)+abs(H*Dydx)+Tiny_I;
           when S =>
              for I in Y'Range loop
                 if abs(Ystart(I)) > 0.0 then -- usando \= 0.0 si lagna di un carattere illegale
                    Yscal(I) := Max(abs(Ystart(I)),abs(Y(I)));
                 else
                    Yscal(I) := Max(1.0, abs(Y(I)));
                 end if;
              end loop;
        end case;
        if N_Xout > 0 and (X_Ini_P < X and X < X_Fin_P) then
          if abs(X-Xsav) > abs(Dxsav_I) then
            if Kount < N_Xout then
              Kount := Kount + 1;
              Xp(Kount) := X;
              DXp(Kount) := H;
              Yp(Kount) := Y;
              Up(Kount) := Der.U;
              Xsav := X;
            end if;
          end if;
        end if;
        --        if (X + H - X_Fine_I)*(X +H -X_Iniz_I) > 0.0 then H := X_Fine_I - X ; end if;
        begin
           case NOS is
              when N => Rkqs(H, Dydx, X, Y, Par);
              when S => Stiff(H, Dydx, X, Y, Par);
           end case;
        exception
           when Stepsize =>
              Rapp := (Nok => Nok, Nbad => Nbad, Nrhs => Nrhs, Kount => Kount);
              raise Stepsize;
        end;
        if Hdid = H then
          Nok := Nok + 1;
        else
          Nbad := Nbad + 1;
        end if;
        if (X - X_Fine_I)*(X_Fine_I - X_Iniz_I) >= 0.0 then
          Ystart := Y;
          if X_Ini_P < X and X < X_Fin_P then
            Kount := Kount + 1;
            Xp(Kount) := X;
            DXp(Kount) := H;
            Yp(Kount) := Y;
            Up(Kount) := Der.U;
          end if;
        end if;
        if abs(Hnext) < Hmin_I then
           Rapp := (Nok => Nok, Nbad => Nbad, Nrhs => Nrhs, Kount => Kount);
           raise Stepsize ;
        end if;
        H := Hnext;
        exit when (X < X_Iniz_I) or (X > X_Fine_I) or (Nstp >= Maxstp_I) or (Kount >= N_Xout);
      end loop;
      Rapp := (Nok => Nok, Nbad => Nbad, Nrhs => Nrhs, Kount => Kount);
    end Ode_Int ;
    procedure Rkck(X,H : in Mio_Float; Y, Dydx : in V_Y; Ytemp, Yerr : out V_Y; Par : Parametri_Ingresso) is
       -- Parametri di rkck
       A2 : Mio_Float := 0.2; A3 : Mio_Float := 0.3; A4 : Mio_Float := 0.6;
       A5 : Mio_Float := 1.0; A6 : Mio_Float := 0.875;
       B21 : Mio_Float := 0.2; B31 : Mio_Float := 3.0/40.0;
       B32 : Mio_Float := 9.0/40.0; B41 : Mio_Float := 0.3;
       B42 : Mio_Float := -0.9; B43 : Mio_Float := 1.2;
       B51 : Mio_Float := -11.0/54.0; B52 : Mio_Float := 2.5;
       B53 : Mio_Float := -70.0/27.0; B54 : Mio_Float := 35.0/27.0;
       B61 : Mio_Float := 1631.0/55296.0; B62 : Mio_Float := 175.0/512.0;
       B63 : Mio_Float := 575.0/13824.0; B64 : Mio_Float := 44275.0/110592.0;
       B65 : Mio_Float := 253.0/4096.0; C1 : Mio_Float := 37.0/378.0;
       C3 : Mio_Float := 250.0/621.0; C4 : Mio_Float := 125.0/594.0;
       C6 : Mio_Float := 512.0/1771.0; DC1 : Mio_Float := C1-2825.0/27648.0;
       DC3 : Mio_Float := C3-18575.0/48384.0; DC4 : Mio_Float := C4-13525.0/55296.0;
       DC5 : Mio_Float := -277.0/14336.0; DC6 : Mio_Float := C6-0.25;
       Der : DY_E_U;
       Ak2,Ak3,Ak4,Ak5,Ak6 : V_Y :=(others => 0.0);
    begin
       Ytemp := Y+B21*H*Dydx;
       Der := RHS(X+A2*H, Ytemp, Par);
       Ak2 := Der.DY;
       Ytemp := Y+H*(B31*Dydx+B32*Ak2);
       Der := RHS(X+A3*H, Ytemp, Par);
       Ak3 := Der.DY;
       Ytemp := Y+H*(B41*Dydx+B42*Ak2+B43*Ak3);
       Der := RHS(X+A4*H, Ytemp, Par);
       Ak4 := Der.DY;
       Ytemp := (Y+H*(B51*Dydx+B52*Ak2+B53*Ak3+B54*Ak4));
       Der := RHS(X+A5*H, Ytemp, Par);
       Ak5 := Der.DY;
       Ytemp := (Y+H*(B61*Dydx+B62*Ak2+B63*Ak3+B64*Ak4+B65*Ak5));
       Der := RHS(X+A6*H, Ytemp, Par);
       Ak6 := Der.DY;
       Ytemp := (Y+H*(C1*Dydx+C3*Ak3+C4*Ak4+C6*Ak6));
       Yerr := H*(DC1*Dydx+DC3*Ak3+DC4*Ak4+DC5*Ak5+DC6*Ak6);
    end Rkck;
    procedure Rkqs(Htry : in Mio_Float; Dydx : in V_Y; X : in out Mio_Float; Y : in out V_Y; Par : Parametri_Ingresso) is
       H : Mio_Float := Htry;
       Errmax, Htemp : Mio_Float;
       -- Parametri di rkqs
       SAFETY : Mio_Float := 0.9; PGROW : Mio_Float := -0.2; PSHRNK : Mio_Float := -0.25;
       ERRCON : Mio_Float := exp((1.0/PGROW)*log(5.0/SAFETY));
       Ytemp, Yerr : V_Y := (others => 0.0);
    begin
       loop
         Rkck(X, H, Y, Dydx, Ytemp, Yerr, Par);
         Nrhs := Nrhs + 5;
         Errmax := Max(abs(Yerr/Yscal) / Eps_I);
         exit when Errmax <= 1.0;
         Htemp := SAFETY*H*Exp(PSHRNK*Log(Errmax));
         H := (H/abs(H)) * max(abs(Htemp),0.1*abs(H));
         if X + H = X then raise Stepsize; end if;
       end loop;
       if errmax > ERRCON then
          Hnext := SAFETY*H*Exp(PGROW*Log(Errmax));
       else
          Hnext := 5.0 * H;
       end if;
       Hdid := H;
       Y := Ytemp;
       X := X + H;
    end Rkqs;
    procedure Stiff(Htry : in Mio_Float; Dydx : in V_Y; X : in out Mio_Float; Y : in out V_Y; Par : Parametri_Ingresso) is 
       MAXTRY : Integer := 40;
       SAFETY : Mio_Float := 0.9; GROW : Mio_Float := 1.5; PGROW : Mio_Float := -0.25; SHRNK : Mio_Float := 0.5;
       PSHRNK : Mio_Float := -1.0/3.0;  ERRCON : Mio_Float := 0.1296;
       GAM : Mio_Float := 1.0/2.0; A21 : Mio_Float := 2.0; A31 : Mio_Float := 48.0/25.0; A32 : Mio_Float := 6.0/25.0;
       C21 : Mio_Float := -8.0; C31 : Mio_Float := 372.0/25.0; C32 : Mio_Float := 12.0/5.0;
       C41 : Mio_Float := -112.0/125.0; C42 : Mio_Float := -54.0/125.0; C43 : Mio_Float := -2.0/5.0;
       B1 : Mio_Float := 19.0/9.0; B2 : Mio_Float := 1.0/2.0; B3 : Mio_Float := 25.0/108.0;
       B4 : Mio_Float := 125.0/108.0; E1 : Mio_Float := 17.0/54.0; E2 : Mio_Float := 7.0/36.0;
       E3 : Mio_Float := 0.0; E4 : Mio_Float := 125.0/108.0; C1X : Mio_Float := 1.0/2.0; C2X : Mio_Float := -3.0/2.0;
       C3X : Mio_Float := 121.0/50.0; C4X : Mio_Float := 29.0/250.0; A2X : Mio_Float := 1.0; A3X : Mio_Float := 3.0/5.0;
       Der : DY_E_U;
       Jac : Da_Jacobi;
       A : M_Y;
       H, Xsav, Xn, Errmax : Mio_Float;
       Err, G1, G2, G3, G4, Ysav : V_Y;
       Superato_MAXTRY : exception;
    begin
       Xsav := X;
       Ysav := Y;
       Jac := Jacobi(Xsav, Ysav, Par);
       H := Htry;
       for Jtry in 1..MAXTRY loop
         A := Jac.M;
         for I in A'Range(1) loop
            A(I,I) := 1.0 / (GAM*H) + A(I,I);
         end loop;
--       LU_Y.Ludcmp(Matr(A));
         G1 := Dydx + H * C1X * Jac.V;
	 --       LU_Y.Lubksb(Matr(A), Vett(G1));
	 G1 := V_Y(Vet_Mat.Solve(Matr(A), Vett(G1)));
         Y := Ysav + A21 * G1;
         XN:= Xsav + A2X * H;
         Der := RHS(X, Y, Par); Nrhs := Nrhs + 1;
         G2 := Der.DY + H * C2X * Jac.V + C21 * G1 / H;
	 --         LU_Y.Lubksb(Matr(A), Vett(G2));
	 G2 := V_Y(Vet_Mat.Solve(Matr(A), Vett(G2)));
         Y  := Ysav + A31 * G1 + A32 * G2;
         XN:= Xsav + A3X * H;
         Der := RHS(X, Y, Par); Nrhs := Nrhs + 1;
         G3 := Der.DY + H * C3X * Jac.V + (C31 * G1 + C32 * G2) / H;
	 --         LU_Y.Lubksb(Matr(A), Vett(G3));
	 G3 := V_Y(Vet_Mat.Solve(Matr(A), Vett(G3)));
         G4 := Der.DY + H * C4X * Jac.V + (C41 * G1 + C42 * G2 + C43 * G3) / H;
	 --         LU_Y.Lubksb(Matr(A), Vett(G4));
	 G4 := V_Y(Vet_Mat.Solve(Matr(A), Vett(G4)));
         Y := Ysav + B1 * G1 + B2 * G2 + B3 * G3 + B4 * G4;
         Err := E1 * G1 + E2 * G2 + E3 * G3 + E4 * G4;
         XN:= Xsav + H;
         if XN = Xsav then raise Stepsize; end if;
         Errmax := Max(abs(Err / Yscal));
         Errmax := Errmax/Eps_I;
         if Errmax <= 1.0 then
           Hdid := H;
           if Errmax > ERRCON then
             Hnext := SAFETY * H * Exp(PGROW * Log(Errmax));
           else
             Hnext := GROW * H;
           end if;
	   X := Xn;
           return;
         else
           Hnext := SAFETY * H * Exp(PSHRNK * Log(Errmax));
           H := (H/abs(H))*(Max(abs(hnext),SHRNK*abs(H)));
         end if;
       end loop;
       raise Superato_MAXTRY;
    end Stiff;
    function Jacobi_Dfdy(IC : Integer; X : Mio_Float; Y : V_Y; Par : Parametri_Ingresso) return V_Y is
       CON : Mio_Float := 1.4;
       CON2 : Mio_Float := CON*CON;
       BIG : Mio_Float := 1.0E30;
       NTAB : Integer := 10;
       SAFE : Mio_Float := 2.0;
       TN : V_Y := (others => 0.0);
       Mat_A : array(1..NTAB,1..NTAB) of V_Y := (others => (others => TN));
       DELT_Y : Mio_Float := Yscal(IC)/10.0;
       HH : Mio_Float := DELT_Y;
       Err : Mio_Float := BIG;
       Fac, Errt : Mio_Float;
       X_P_HH, X_M_HH : V_Y := Y;
    begin
       X_P_HH(IC) := Y(IC) + HH;
       X_M_HH(IC) := Y(IC) - HH;
       Mat_A(1,1) := (RHS(X, X_P_HH, Par).DY - RHS(X, X_M_HH, Par).DY) / (2.0*HH);
       for I in 2..NTAB loop
         HH := HH / CON;
         X_P_HH(IC) := Y(IC) + HH;
         X_M_HH(IC) := Y(IC) - HH;
         Mat_A(1,I) := (RHS(X, X_P_HH, Par).DY - RHS(X, X_M_HH, Par).DY) / (2.0*HH);
         Fac := CON2;
         for J in 2..I loop
           Mat_A(J,I) := (Mat_A(J-1,I)*Fac - Mat_A(J-1,I-1))/(Fac - 1.0);
           Fac := CON2 * Fac;
           Errt := Max(Max(abs(Mat_A(J,I)-Mat_A(J-1,I))),Max(abs(Mat_A(J,I)-Mat_A(J-1,I-1))));
           if Errt <= Err then
              Err := Errt;
              TN := Mat_A(J,I);
           end if;
         end loop;
         exit when Max(abs(Mat_A(I,I)-Mat_A(I-1,I-1))) >= SAFE * Err;
       end loop;
       return TN;
    end Jacobi_Dfdy;
    function Jacobi_Dfdx(X : Mio_Float; Y : V_Y; Par : Parametri_Ingresso) return V_Y is
       CON : Mio_Float := 1.4;
       CON2 : Mio_Float := CON*CON;
       BIG : Mio_Float := 1.0E30;
       NTAB : Integer := 10;
       SAFE : Mio_Float := 2.0;
       TN : V_Y := (others => 0.0);
       Mat_A : array(1..NTAB,1..NTAB) of V_Y := (others => (others => TN));
       Delt_X : Mio_Float := X_Fine_I / 100.0;
       HH : Mio_Float := Delt_X;
       Err : Mio_Float := BIG;
       Fac, Errt : Mio_Float;
    begin
       Mat_A(1,1) := (RHS(X+HH, Y, Par).DY - RHS(X-HH, Y, Par).DY) / (2.0*HH);
       for I in 2..NTAB loop
         HH := HH / CON;
         Mat_A(1,I) := (RHS(X+HH, Y, Par).DY - RHS(X-HH, Y, Par).DY) / (2.0*HH);
         Fac := CON2;
         for J in 2..I loop
           Mat_A(J,I) := (Mat_A(J-1,I)*Fac - Mat_A(J-1,I-1))/(Fac - 1.0);
           Fac := CON2 * Fac;
           Errt := Max(Max(abs(Mat_A(J,I)-Mat_A(J-1,I))),Max(abs(Mat_A(J,I)-Mat_A(J-1,I-1))));
           if Errt <= Err then
              Err := Errt;
              TN := Mat_A(J,I);
           end if;
         end loop;
         exit when Max(abs(Mat_A(I,I)-Mat_A(I-1,I-1))) >= SAFE * Err;
       end loop;
       return TN;
    end Jacobi_Dfdx;
    function Jacobi(X : Mio_Float; Y : V_Y; Par : Parametri_Ingresso) return Da_Jacobi is
       Vett : V_Y;
       Ris : Da_Jacobi;
       begin
          for IC in Y'Range loop
             Vett := Jacobi_Dfdy(IC, X, Y, Par);
             for IR in Y'Range loop
                Ris.M(IR,IC) := - Vett(IR);
             end loop;
          end loop;
          Ris.V := Jacobi_Dfdx(X, Y, Par);
          return Ris;
    end Jacobi;
    function X_P(N : Positive) return Mio_Float is
    begin
       return Xp(N);
    end X_P;
    function Y_P(N : Positive) return V_Y is
    begin
       return Yp(N);
    end Y_P;
    function U_P(N : Positive) return V_U is
    begin
       return Up(N);
    end U_P;
    function Ris_Dx return Risultati is
       R : Risultati;
    begin
       for I in Xp'Range loop
          R.Asc(I) := Xp(I);
          R.Ord(I) := DXp(I);
       end loop;
       return R;
    end Ris_Dx;
    function Ris_Y(K : Positive) return Risultati is
       R : Risultati;
    begin
       for I in Xp'Range loop
          R.Asc(I) := Xp(I);
          R.Ord(I) := Yp(I)(K);
       end loop;
       return R;
    end Ris_Y;
    function Ris_U(K : Positive) return Risultati is
       R : Risultati;
    begin
       for I in Xp'Range loop
          R.Asc(I) := Xp(I);
          R.Ord(I) := Up(I)(K);
       end loop;
       return R;
    end Ris_U;
    procedure Plot_Ris(R : Risultati; Scala : Mio_Float) is
    begin
       for I in 1..Rapp.Kount loop
          Put(R.Asc(I)); Put(" "); Put(Scala*R.Ord(I));
          New_Line;
       end loop;
    end Plot_Ris;
    procedure Plot_Curva(Curva : Risultati; Scala : Mio_Float; Nome_file : String := ""; Gnplt : Gnu_Plot := Gnuplot) is
       Nuovo_file : File_Type;
    begin
       if Nome_File = "" then
          Set_Output(Standard_Output);
       else
          Create(Nuovo_File,Out_File,Nome_File);
          Set_Output(Nuovo_File);
       end if;
       if Gnplt = Gnuplot then
          Put("set term postscript color"); New_Line;
          Put("set output '"); Put(Nome_File); Put(".ps'"); New_Line;
          Put("plot'-' title '"); Put(Nome_File); Put("' with lines"); New_Line;
       end if;
       Plot_Ris(Curva,Scala);
       if Nome_File /= "" then
          Close(Nuovo_File);
       end if;
    end Plot_Curva;
    procedure Plot_Curva(Curva : Risultati; Nome_file : String := ""; Gnplt : Gnu_Plot := Gnuplot) is
       Nuovo_file : File_Type;
       Scala : Mio_Float := 1.0; --  / Max(abs(Curva.Ord));
    begin
       if Nome_File = "" then
          Set_Output(Standard_Output);
       else
          Create(Nuovo_File,Out_File,Nome_File);
          Set_Output(Nuovo_File);
       end if;
       if Gnplt = Gnuplot then
          Put("set term postscript color"); New_Line;
          Put("set output '"); Put(Nome_File); Put(".ps'"); New_Line;
          Put("plot'-' title '"); Put(Nome_File); Put("' with lines"); New_Line;
       end if;
       Plot_Ris(Curva,Scala);
       if Nome_File /= "" then
          Close(Nuovo_File);
       end if;
    end Plot_Curva;
    procedure Scrivi_rapporto(Nome_file : String := "") is
       Nuovo_file : File_Type;
    begin
       if Nome_File = "" then
          Set_Output(Standard_Output);
       else
          Create(Nuovo_File,Out_File,Nome_File);
          Set_Output(Nuovo_File);
       end if;
       Put(" Passi riusciti = ");
       Put(Rapp.Nok); New_Line;
       Put(" Passi sbagliati = ");
       Put(Rapp.Nbad); New_Line;
       Put(" Valutazioni del termine noto = ");
       Put(Rapp.Nrhs); New_Line;
       Put(" Valori intermedi = ");
       Put(Rapp.Kount); New_Line;
       Put(" Tempo di calcolo: "); Put(Mio_Float(Clock-Tempo),Fore=>3,Aft=>2); Put(" secondi"); New_Line;
       if Nome_File /= "" then
          Close(Nuovo_File);
       end if;
    end Scrivi_Rapporto;
    procedure Put(V : V_Y) is
    begin
       for I in V'Range loop
          Put(V(I)); Put(" ");
       end loop;
       New_Line;
    end Put;
end LS.ODE;

with Ada.Numerics; use Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;
with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with LS.Integer_IO; use LS.Integer_IO;
with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;
with Ada.Calendar; use Ada.Calendar;
with LS.Utili; use LS.Utili;
--with LS.LU;
--with Ada.Command_Line; use Ada.Command_Line;
with LS.ODE; 
procedure LS.Eqz_Diff is
   N_Xout : Positive := 1000;
   N_Var : Positive := 3;
   N_Usc : Positive := 2;
   procedure Eqz_Diff_1 is
      type Param_Ingr is new Vettore(1..6);
      Prm : Param_Ingr :=(-0.013,-1000.0,-2500.0,-0.013,-1000.0,-2500.0);
      package Nuova_ODE is new LS.ODE(N_X => N_Xout, N_Y => N_Var, N_U => N_Usc, 
                                      Parametri_Ingresso => Param_Ingr); use Nuova_ODE;
      function Calc_Eq_Diff(T : Mio_Float; Y : V_Y; Pr : Param_Ingr) return DY_E_U is
         D : DY_E_U;
         dy1 : Mio_Float := Pr(1)*Y(1) + Pr(2)*Y(1)*Y(3) + T;
         dy2 : Mio_Float := Pr(3)*Y(2)*Y(3) - T**3;
         dy3 : Mio_Float := Pr(4)*Y(1) + Pr(5)*Y(1)*Y(3) +Pr(6)*Y(2)*Y(3);
      begin
         D.DY(1) := Dy1;
         D.DY(2) := Dy2;
         D.DY(3) := Dy3;
	 D.U(1) := Dy1*Dy2;
	 D.U(2) := Dy3*Dy2;
         return D;
      end Calc_Eq_Diff;
      Val_In : V_Y := (1=>1.0, 2=>1.0, 3=>0.0);
      NOS : Normale_O_Stiff := N;
   begin
      Cambia_Parametri (H1 => 2.9E-04);
      Asse_X(X_Iniz => 0.0, X_Fine => 1.0, Dxsav => 0.01);
      Val_Ini(Y_Ini => Val_In);
      begin
         Ode_Int(Calc_Eq_Diff'Access, NOS, Prm);
      exception
         when Stepsize => New_Line; Put("Passo d'integrazione troppo piccolo"); New_Line;
      end;
      Scrivi_Rapporto;
      Scrivi_Rapporto(Nome_File => "Eq_Diff_1.Log");
      Plot_Curva(Ris_Dx, Nome_File => "dx_1.ris");
      Plot_Curva(Ris_Y(1), Nome_File => "y1_1.ris", Gnplt => Nognuplot);
      Plot_Curva(Ris_Y(2), Nome_File => "y2_1.ris", Gnplt => Nognuplot);
      Plot_Curva(Ris_Y(3), Nome_File => "y3_1.ris");
      Plot_Curva(Ris_U(2), Nome_File => "u2_1.ris");
      Plot_Curva(Ris_U(1), Nome_File => "u1_1.ris");
   end Eqz_Diff_1;
   procedure Eqz_Diff_2 is
      type Param_Ingr is new Vettore(1..5);
      Prm : Param_Ingr :=(-0.013,-1000.0,-2500.0,-0.013,-1000.0);
      package Nuova_ODE is new LS.ODE(N_X => N_Xout, N_Y => N_Var, N_U => N_Usc, 
                                      Parametri_Ingresso => Param_Ingr); use Nuova_ODE;
      function Calc_Eq_Diff(T : Mio_Float; Y : V_Y; Pr : Param_Ingr) return DY_E_U is
         D : DY_E_U;
         dy1 : Mio_Float := Pr(1)*Y(1) + Pr(2)*Y(1)*Y(3);
         dy2 : Mio_Float := Pr(3)*Y(2)*Y(3);
         dy3 : Mio_Float := Pr(4)*Y(1) + Pr(5)*Y(1)*Y(3) +Pr(3)*Y(2)*Y(3);
      begin
         D.DY(1) := Dy1;
         D.DY(2) := Dy2;
         D.DY(3) := Dy3;
	 D.U(1) := Dy1*Dy2;
	 D.U(2) := Dy3*Dy2;
         return D;
      end Calc_Eq_Diff;
      Val_In : V_Y := (1=>1.0, 2=>1.0, 3=>0.0);
      NOS : Normale_O_Stiff := N;
   begin
      Cambia_Parametri (H1 => 2.9E-04);
      Asse_X(X_Iniz => 0.0, X_Fine => 1.0, Dxsav => 0.01);
      Val_Ini(Y_Ini => Val_In);
      begin
         Ode_Int(Calc_Eq_Diff'Access, NOS, Prm);
      exception
         when Stepsize => New_Line; Put("Passo d'integrazione troppo piccolo"); New_Line;
      end;
      Scrivi_Rapporto;
      Scrivi_Rapporto(Nome_File => "Eq_Diff_2.Log");
      Plot_Curva(Ris_Dx, Nome_File => "dx_2.ris");
      Plot_Curva(Ris_Y(1), Nome_File => "y1_2.ris");
      Plot_Curva(Ris_Y(2), Nome_File => "y2_2.ris");
      Plot_Curva(Ris_Y(3), Nome_File => "y3_2.ris");
      Plot_Curva(Ris_U(2), "u2_2.ris", Nognuplot);
      Plot_Curva(Ris_U(1), "u1_2.ris", Nognuplot);
   end Eqz_Diff_2;
begin
   Eqz_Diff_1; 
   Eqz_Diff_2;
end LS.Eqz_Diff;

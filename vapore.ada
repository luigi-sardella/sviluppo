with Ada.Numerics; use Ada.Numerics;
with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with LS.Integer_IO; use LS.Integer_IO;
with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with LS.Solve1D_Gen;
with LS.Derivate_Numeriche;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
--    Proprietà fisiche del vapor d'acqua
package LS.Vapore is
   type Ent_Tit is record
      Entalpia, Titolo : Mio_Float;
   end record;
   type Tem_Tit is record
      Temperatura, Titolo : Mio_Float;
   end record;
   type Ent_Ent is record
      Entalpia, Entropia : Mio_Float;
   end record;
   type Ene_Ent is record
      Energia, Entalpia : Mio_Float;
   end record;
   -- Entalpia del vapore, in Cal/kg
   function Hv_Cal(P, T : Mio_Float) return Mio_Float;
   -- Entalpia del vapore, in J/kg
   function Hv_J(P, T : Mio_Float) return Mio_Float;
   -- Entropia del vapore, in Cal/(K kg)
   function Sv_Cal(P, T : Mio_Float) return Mio_Float;
   -- Entropia del vapore, in J/(K kg)
   function Sv_J(P, T : Mio_Float) return Mio_Float;
   -- Volume specifico del vapore, in m^3/kg
   function Vol_V(P, T : Mio_Float) return Mio_Float;
   -- Densità del vapore, in kg/m^3
   function Dens_V(P, T : Mio_Float) return Mio_Float;
   -- Entalpia del liquido, in J/kg
   function Hl_J(T : Mio_Float) return Mio_Float;
   -- Entropia del liquido, in J/(K kg)
   function Sl_J(T : Mio_Float) return Mio_Float;
   -- Volume specifico del liquido, in m^3/kg
   function Vol_L(T : Mio_Float) return Mio_Float;
   -- Densità del liquido, in kg/m^3
   function Dens_L(T : Mio_Float) return Mio_Float;
   function Pres_Sat(T : Mio_Float) return Mio_Float;
   function Tem_Sat(P : Mio_Float) return Mio_Float;
   -- Entalpia e titolo del vapore bagnato, in funzione di pressione ed entropia
   function H_J(P, S : Mio_Float) return Ent_Tit;
   -- Entalpia ed entropia in funzione di pressione e titolo
   function HS_Px(P, Titolo : Mio_Float) return Ent_Ent;
   function Tem_Tit_HP(H, P : Mio_Float) return Tem_Tit;
   function EH_TS(T, S : Mio_Float) return Ene_Ent;
   function E_TS(T, S : Mio_Float) return Mio_Float;
   function H_TS(T, S : Mio_Float) return Mio_Float;
   function Gamma(Dens, T : Mio_Float) return Mio_Float;
   function Vel_Suono(P, S : Mio_Float) return Mio_Float;
   procedure Stampa; procedure Non_Stampare;
   C_Liq : constant Mio_Float := 4186.8;
   T0 : constant Mio_Float := 273.16;
private
   type A_B is record D, H, P, S, T : Mio_Float; end record;
   package Solve_Float is new LS.Solve1D_Gen(Reale => Mio_Float, Param => Mio_Float);
   use Solve_Float;
   package Solve_AB is new LS.Solve1D_Gen(Reale => Mio_Float, Param => A_B);
   use Solve_AB;
   package D_N is new LS.Derivate_Numeriche(Reale => Mio_Float, Param => Mio_Float);
   use D_N;
   function Calc_T_Sat(T_Sat, Press : Mio_Float) return Mio_Float;
   function Calc_Tcl(T, S : Mio_Float) return Mio_float;
   function Calc_Tem_Da_P_S(T : Mio_Float; PS : A_B) return Mio_Float;
   function Calc_Pre_Da_S_T(P : Mio_Float; ST : A_B) return Mio_Float;
   function Calc_Pre_Da_D_T(P : Mio_Float; DT : A_B) return Mio_Float;
end LS.Vapore;
package body LS.Vapore is
   -- Entalpia del vapore, in Cal/kg
   function Hv_Cal(P, T : Mio_Float) return Mio_Float is
      P_Bar : Mio_Float := P * 1.0E-5;
   begin
      return (474.89-(1.594_487_658_25E+34*T**(-14)+1.837_612_073_21E+81*T**(-31.6))
     *P_bar**3-36_297_348.711_7*P_bar*T**(-2.82)-0.000_045_757*T**2+0.717E-7*T**3
     +0.45493*T);
   end Hv_Cal;
   -- Entalpia del vapore, in J/kg
   function Hv_J(P, T : Mio_Float) return Mio_Float is begin
      return C_Liq*Hv_Cal(P, T);
   end Hv_J;
   -- Entropia del vapore, in Cal/(K kg)
   function Sv_Cal(P, T : Mio_Float) return Mio_Float is
      P_Bar : Mio_Float := P * 1.0E-5;
   begin
      return (-0.909_864_565_66-(1.488_188_481_03E+34*T**(-15)+1.781_243_604_71E+81
     *T**(-32.6))*P_bar**3-26_795_424.965_2*T**(-3.82)*P_bar+0.000_000_107_55*T**2
     +0.454_93*log(T)-0.110_226_5*log(P_bar)-0.000_091_514*T);
   end Sv_Cal;
   -- Entropia del vapore, in J/(K kg)
   function Sv_J(P, T : Mio_Float) return Mio_Float is begin
      return C_Liq*Sv_Cal(P, T);
   end Sv_J;
   -- Volume specifico del vapore, in m^3/kg
   function Vol_V(P, T : Mio_Float) return Mio_Float is
      P_Bar : Mio_Float := P * 1.0E-5;
   begin
      return (-(3.147_273_936_84E+29*T**(-14)+1.668_940_592_26E+76*T**(-31.6))
     *P_bar**2+4.647_062_730_96E-3*T/P_bar-937.766_962_401*T**(-2.82));
   end Vol_V;
   -- Densità del vapore da pressione e temperatura, in kg/m^3
   function Dens_V(P, T : Mio_Float) return Mio_Float is begin
      return 1.0/Vol_V(P, T);
   end Dens_V;
   -- Densità del vapore da pressione ed entropia, in kg/m^3
   function Dv_PS(P, S : Mio_Float) return Mio_Float;
   -- Entalpia del liquido, in Cal/kg
   function Hv_Cal(T : Mio_Float) return Mio_Float is
   begin
      return T - T0;
   end Hv_Cal;
   -- Entalpia del liquido, in J/kg
   function Hl_J(T : Mio_Float) return Mio_Float is begin
      return C_Liq * (T - T0);
   end Hl_J;
   -- Entropia del liquido, in J/(K kg)
   function Sl_J(T : Mio_Float) return Mio_Float is begin
      return C_Liq * Log(T / T0);
   end Sl_J;
   -- Volume specifico del liquido, in m^3/kg
   function Vol_L(T : Mio_Float) return Mio_Float is
   begin
      return 0.001;
   end Vol_L;
   -- Densità del liquido, in kg/m^3
   function Dens_L(T : Mio_Float) return Mio_Float is begin
      return 1000.0;
   end Dens_L;
   function Pres_Sat(T : Mio_Float) return Mio_Float is
      P_Bar : Mio_Float := 10.0**((20.211_32-4.5*Log(T,10.0)-2_980.46/T
                                   -0.002_78*T+0.000_002_825*T**2));
   begin
      return P_bar * 1.0E5;
   end Pres_Sat;
   function Calc_T_Sat(T_Sat, Press : Mio_Float) return Mio_Float is begin
      return Pres_Sat(T_Sat) - Press;
   end Calc_T_Sat;
   function Tem_Sat(P : Mio_Float) return Mio_Float is
   begin
      return Solve(Calc_T_Sat'Access, 1.0, 1000.0, P, "Tem_Sat");
   end Tem_Sat;
   function Calc_Tem_Da_H_P(T : Mio_Float; HP : A_B) return Mio_Float is
   begin
      return HP.H - Hv_J(HP.P, T);
   end Calc_Tem_Da_H_P;
   function Tem_Tit_HP(H, P : Mio_Float) return Tem_Tit is
      Tsat : Mio_Float := Tem_Sat(P);
      Hsat : Mio_Float := Hv_J(P, Tsat);
      HP : A_B := (H => H, P => P, others => 0.0);
   begin
      if H <= Hsat then
         return (Tsat, (H - Hl_J(Tsat)) / ( Hv_J(P, Tsat) - Hl_J(Tsat)));
      else
         return (Solve(Calc_Tem_Da_H_P'Access, 0.5*T0, T0+900.0, HP,"Tem_Tit_HP"), 1.0);
      end if;
   end Tem_Tit_HP;
   function Calc_Tem_Da_P_S(T : Mio_Float; PS : A_B) return Mio_Float is
   begin
      return PS.S - Sv_J(PS.P,T);
   end Calc_Tem_Da_P_S;
   function H_J(P, S : Mio_Float) return Ent_Tit is
      Temp, Tsat, Titolo : Mio_Float;
      PS : A_B := (P => P, S => S, others => 0.0);
   begin
      Temp := Solve(Calc_Tem_Da_P_S'Access, 0.5*T0, T0+900.0, PS, "H_J, Temp");
      Tsat := Tem_Sat(P);
      if Temp > Tsat then
         return (Hv_J(P, Temp), 1.0);
      else
         Titolo := (S - Sl_J(Tsat)) / ( Sv_J(P, Tsat) - Sl_J(Tsat));
         return ((1.0 - Titolo)*Hl_J(Tsat) + Titolo*Hv_J(P, Tsat), Titolo);
      end if;
   end H_J;
   function HS_Px(P, Titolo : Mio_Float) return Ent_Ent is
      Tsat : Mio_Float := Tem_Sat(P);
   begin
      return ((1.0 - Titolo)*Hl_J(Tsat) + Titolo*Hv_J(P, Tsat),
              (1.0 - Titolo)*Sl_J(Tsat) + Titolo*Sv_J(P, Tsat) );
   end HS_Px;
   function Dv_PS(P, S : Mio_Float) return Mio_Float is
      PS : A_B := (P => P, S => S, others => 0.0);
      Temp : Mio_Float := Solve(Calc_Tem_Da_P_S'Access, 0.5*T0, T0+900.0, PS,"Dv_PS");
   begin
      return Dens_V(P, Temp);
   end Dv_PS;
   function Vel_Suono(P, S : Mio_Float) return Mio_Float is
      C2 : Mio_Float := Derivata(Dv_PS'Access, P, 0.2*P, S).Deriv;
   begin
      return Sqrt(1.0 / C2);
   end Vel_Suono;
   function Calc_Pre_Da_S_T(P : Mio_Float; ST : A_B) return Mio_Float is
   begin
      return Sv_J(P, ST.T) - ST.S;
   end Calc_Pre_Da_S_T;
   function Calc_Pre_Da_D_T(P : Mio_Float; DT : A_B) return Mio_Float is
   begin
      return Dens_V(P, DT.T) - DT.D;
   end Calc_Pre_Da_D_T;
   function Calc_Tcl(T, S : Mio_Float) return Mio_Float is
   begin
      return Sv_J(Pres_Sat(T), T) - S;
   end Calc_Tcl;
   function EH_TS(T, S : Mio_Float) return Ene_Ent is
      Tcl : Mio_Float := Solve(Calc_Tcl'Access, 0.5*T0, T0+900.0, S, "EH_TS, Tcl");
      ST : A_B := (S => S, T => T, others => 0.0);
      P, Titolo : Mio_Float;
      Ris : Ene_Ent;
   begin
      if T > Tcl then -- sopra la curva limite
         P := Solve(Calc_Pre_Da_S_T'Access, 100.0, 500.0E5, ST, "EH_TS, P");
         Ris.Entalpia := Hv_J(P, T);
         Ris.Energia := Ris.Entalpia - P*Vol_V(P, T);
      else -- sotto la CL
         P := Pres_Sat(T);
         Titolo := (S - Sl_J(T)) / (Sv_J(P, T) - Sl_J(T));
         Ris.Entalpia := Titolo*Hv_J(P, T) + ( 1.0 - Titolo)*Hl_J(T);
         Ris.Energia := Ris.Entalpia
           - P*(Titolo*Vol_V(P, T) + ( 1.0 - Titolo)*Vol_L(T));
      end if;
      return Ris;
   end EH_TS;
   function E_TS(T, S : Mio_Float) return Mio_Float is
   begin
      return EH_TS(T, S).Energia;
   end E_TS;
   function H_TS(T, S : Mio_Float) return Mio_Float is
   begin
      return EH_TS(T, S).Entalpia;
   end H_TS;
   function Gamma(Dens, T : Mio_Float) return Mio_Float is
      DT : A_B := (D => Dens, T => T, others => 0.0);
      Dens_Sat : Mio_Float := Dens_V(Pres_Sat(T), T);
      P, S, Titolo, DH_DT, DE_DT : Mio_Float;
   begin
      if Dens < Dens_Sat then -- fuori dalla curva limite
         P := Solve(Calc_Pre_Da_D_T'Access, 1.0e5, DT);
         S := Sv_J(P, T);
      else
         Titolo := (1.0/Dens - 1.0/Dens_L(T)) /
           (1.0/Dens_Sat - 1.0/Dens_L(T));
         S := Titolo*Sv_J(Pres_Sat(T), T) + (1.0 - Titolo)*Sl_J(T);
      end if;
      DH_DT := Derivata(H_TS'Access, T, 0.2*T, S).Deriv;
      DE_DT := Derivata(E_TS'Access, T, 0.2*T, S).Deriv;
      return DH_DT / DE_DT;
   end Gamma;
   procedure Stampa is
   begin
      Solve_Float.Stampa;
      Solve_AB.Stampa;
   end Stampa;
   procedure Non_Stampare is
   begin
      Solve_Float.Non_Stampare;
      Solve_AB.Non_Stampare;
   end Non_Stampare;
end LS.Vapore;

with Ada.Numerics; use Ada.Numerics;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with LS.Integer_IO; use LS.Integer_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with LS.Vapore; use LS.Vapore;
procedure LS.Prova_Vap is
   T : Mio_Float := 100.0 + T0;
   P : Mio_Float := 0.030E5;
   S : Mio_Float := 8500.0;
   V : Mio_Float := 0.5;
   P1 : Mio_Float := 10_000.0;
   P2 : Mio_Float := 62_000.0;
   H1 : Mio_Float := 2_000_000.0;
   H2 : Mio_Float := 2_790_000.0;
   procedure Stampa_Gamma(T, V : Mio_Float) is begin
      Put(" v = ");Put(V, Fore=>2,Aft=>2);Put("mc/kg; T = ");
      Put(T, Fore=>2,Aft=>2);
      Put("°C");Put("; Gamma = "); Put(Gamma(1.0/V, T + T0));
      New_Line;
   end Stampa_Gamma;
begin
   Put(" T = "); Put(T); Put(" K -> P = "); Put(Pres_Sat(T)); Put(" Pa"); New_Line;
   Put(" P = "); Put(P); Put(" Pa -> T = "); Put(Tem_Sat(P)); Put(" K"); New_Line;
   Put(" T = "); Put(T); Put(" K ; P = "); Put(P); Put(" Pa -> H = ");
   Put(Hv_Cal(P,T)); Put(" Cal"); New_Line;
   Put(" T = "); Put(T); Put(" K ; P = "); Put(P); Put(" Pa -> S = ");
   Put(Sv_Cal(P,T)); Put(" Cal/K"); New_Line;
   Put(" T = "); Put(T); Put(" K ; P = "); Put(P); Put(" Pa -> D = ");
   Put(Dens_V(P,T)); Put(" kg/mc"); New_Line;
   Put(" S = "); Put(S); Put(" J/kg K ; P = "); Put(P1); Put(" Pa "); New_Line;
   Put("      -> x = ");Put(H_J(P1,S).titolo);Put(" ; H = ");
   Put(H_J(P1,S).Entalpia);Put(" J/kg"); New_Line;
   Put(" S = "); Put(S); Put(" J/kg K ; P = "); Put(P2); Put(" Pa "); New_Line;
   Put("      -> x = ");Put(H_J(P2,S).titolo);Put(" ; H = ");
   Put(H_J(P2,S).Entalpia);Put(" J/kg");
   Put(" ; C =");Put(Vel_Suono(P2,S));Put(" m/s");New_Line;
   Put(" H = ");Put(H1);Put(" J/kg ; P = ");Put(P2); Put(" Pa "); New_Line;
   Put("      -> x = ");Put(Tem_Tit_HP(H1,P2).titolo);Put(" ; T = ");
   Put(Tem_Tit_HP(H1,P2).Temperatura);Put(" K");New_Line;
   Put(" H = ");Put(H2);Put(" J/kg ; P = ");Put(P2); Put(" Pa "); New_Line;
   Put("      -> x = ");Put(Tem_Tit_HP(H2,P2).titolo);Put(" ; T = ");
   Put(Tem_Tit_HP(H2,P2).Temperatura);Put(" K");New_Line;
   -- Non_Stampare;
   Stampa_Gamma(100.0, 2.0);
   Stampa_Gamma(150.0, 20.0);
   Stampa_Gamma(600.0, 0.083);
   Stampa_Gamma(80.0, 3.0);
   Stampa_Gamma(800.0,0.03);
   Stampa_Gamma(800.0,0.9);
   Stampa_Gamma(143.6,0.4622);
   Put(" T = 374 C -> P = "); Put(Pres_Sat(374.0 + T0)*1.0E-5); Put(" bara"); New_Line;   
   Put(" T = 350 C -> P = "); Put(Pres_Sat(350.0 + T0)*1.0E-5); Put(" bara"); New_Line;   
   Put(" T = 200 C -> P = "); Put(Pres_Sat(200.0 + T0)*1.0E-5); Put(" bara"); New_Line;   
end LS.Prova_Vap;


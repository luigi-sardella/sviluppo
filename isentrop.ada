with Ada.Numerics; use Ada.Numerics;
with LS.Numerico; use LS.Numerico;
with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;
with LS.Solve1D; use LS.Solve1D;
package LS.Flusso_Isentropico is
   function P0P_M( P0, P, Gam : Mio_Float) return Mio_Float;
   function M_T0T( M, Gam : Mio_Float) return Mio_Float;
   function M_P0P( M, Gam : Mio_Float) return Mio_Float;
   function T_C( T, R, Gam : Mio_Float) return Mio_Float;
   function M_AAg( M, Gam : Mio_Float) return Mio_Float;
   function AAg_M(A, Ag, Gam, M_estremo : Mio_Float ) return Mio_Float;
   function UsuGam(Gam : Mio_Float) return Mio_Float;
   function Vel_Suono( T, R, Gam : Mio_Float) return Mio_Float renames T_C;
private
   function Da_Risolvere(M : Mio_Float) return Mio_Float;
   Gamma, AAg : Mio_Float;
   Err_Aag_M : exception;
end LS.Flusso_Isentropico;

package body LS.Flusso_Isentropico is

   function UsuGam(Gam : Mio_Float) return Mio_Float is
   begin
      return 1.0 / ( 1.0 - 1.0/Gam );
   end UsuGam;

   function P0P_M( P0, P, Gam : Mio_Float) return Mio_Float is
    usgm : Mio_Float := UsuGam(Gam);
   begin
      return Sqrt( (2.0/(Gam-1.0)) * ((P0/P)**(1.0/usgm) -1.0) );
   end P0P_M;

   function M_T0T( M, Gam : Mio_Float) return Mio_Float is
   begin
   return 1.0 + (Gam-1.0) * M**2 / 2.0;
   end M_T0T;

   function M_P0P( M, Gam : Mio_Float) return Mio_Float is
    usgm : Mio_Float := UsuGam(Gam);
   begin
      return M_T0T(M,Gam)**usgm;
   end M_P0P;

   function T_C( T, R, Gam : Mio_Float) return Mio_Float is
   begin
      return Sqrt( Gam * R * T );
   end T_C;

   function M_AAg( M, Gam : Mio_Float) return Mio_Float is
   begin
     return ((2.0/(Gam+1.0))* M_T0T(M,Gam))**((Gam+1.0)/(2.0*(Gam-1.0))) / M;
   end M_AAg;

   function AAg_M(A, Ag, Gam, M_estremo : Mio_Float ) return Mio_Float is
   begin
      AAg := A / Ag;
      Gamma := Gam;
      if M_Estremo < 1.0 then
         return Solve2(Da_Risolvere'Access, M_Estremo, 1.0);
      elsif M_Estremo > 1.0 then
         return Solve2(Da_Risolvere'Access, 1.0, M_Estremo);
      else
         raise Err_AAg_M;
      end if;
   end AAg_M;

   function Da_Risolvere(M : Mio_Float) return Mio_Float is
      begin
         return M_AAg(M, Gamma) - AAg;
   end Da_Risolvere;

end LS.Flusso_Isentropico;


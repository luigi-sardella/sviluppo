with Ada.Numerics; use Ada.Numerics;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with LS.Numerico; use LS.Numerico;
with LS.Float_IO; use LS.Float_IO;
with LS.Integer_IO; use LS.Integer_IO;
with LS.Funzioni_Elementari; use LS.Funzioni_Elementari;
with LS.Utili; use LS.Utili;
package LS.Pdc_Diff is -- Idelchik pag. 250 e 284
   function Coeff_Phi(Angolo, Reynolds : Mio_Float) return Mio_Float;
   function Perdita_Carico(Semi_Ang, Area_In, Area_Fin, Dens, Visc_D, Portata : Mio_Float) return Mio_Float;
end LS.Pdc_Diff;
package body LS.Pdc_Diff is
   function Coeff_Phi(Angolo, Reynolds : Mio_Float) return Mio_Float is
      Rey_1 : Mio_Float := 0.5E5;
      Rey_2 : Mio_Float := 2.0E5;
      Rey_3 : Mio_Float := 6.0E5;
      Angoli : Vettore := (0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 40.0, 45.0, 50.0, 60.0, 80.0, 140.0, 180.0);
      Phi_1 : Vettore := (0.0, 0.12, 0.26, 0.35, 0.45, 0.58, 0.75, 0.90, 0.95, 0.98, 1.0, 1.02, 1.0, 1.0);
      Phi_2 : Vettore := (0.0, 0.08, 0.15, 0.24, 0.32, 0.43, 0.60, 0.82, 0.88, 0.93, 0.95, 0.95, 0.97, 0.99);
      Phi_3 : Vettore := (0.0, 0.04, 0.09, 0.18, 0.25, 0.37, 0.52, 0.77, 0.82, 0.88, 0.91, 0.95, 0.97, 0.98);
      Risult, Inter_A, Inter_B : Mio_Float;
   begin
      if Reynolds < Rey_1 then
	 Risult := Interpola(Angoli, Phi_1, Angolo);
      elsif Rey_1 <= Reynolds and Reynolds < Rey_2 then
	 Inter_A := Interpola(Angoli, Phi_1, Angolo);
	 Inter_B := Interpola(Angoli, Phi_2, Angolo);
	 Risult := Interpola(Rey_1, Inter_A, Rey_2, Inter_B, Reynolds);
      elsif Rey_2 <= Reynolds and Reynolds < Rey_3 then
	 Inter_A := Interpola(Angoli, Phi_2, Angolo);
	 Inter_B := Interpola(Angoli, Phi_3, Angolo);
	 Risult := Interpola(Rey_2, Inter_A, Rey_3, Inter_B, Reynolds);
      elsif Reynolds >= Rey_3 then
	 Risult := Interpola(Angoli, Phi_3, Angolo);
      end if;
      return Risult;
   end Coeff_Phi;
   function Perdita_Carico(Semi_Ang, Area_In, Area_Fin, Dens, Visc_D, Portata : Mio_Float) return Mio_Float is
      Vel_In : Mio_Float := Portata/(Dens*Area_In); 
      Rey_In : Mio_Float := Portata/(Visc_D*Sqrt(Pi*Area_In/4.0)); -- proprio così
      Rey_Fin : Mio_Float := Portata/(Visc_D*Sqrt(Pi*Area_Fin/4.0));
      Rey_M : Mio_Float := (Rey_In + Rey_Fin)/2.0;
      Lambda_In : Mio_Float := 1.0/(1.8*Log(Rey_In)/Log(10.0) - 1.64)**2;      
      Lambda_Fin : Mio_Float := 1.0/(1.8*Log(Rey_Fin)/Log(10.0) - 1.64)**2;      
      Lambda_M : Mio_Float := (Lambda_In + Lambda_Fin)/2.0;
      Csi_Attr : Mio_Float := Lambda_M*(1.0 - (Area_In/Area_Fin)**2)*Tan(Pi*Semi_Ang/180.0)/8.0;
      Csi_Espn : Mio_Float := Coeff_Phi(2.0*Semi_Ang, Rey_M)*(1.0 - Area_In/Area_Fin)**2;      
 -- Ma i manuali Hoepli I e II ed. invertono i rapporti delle aree; come pure una tabella da Internet. Mi fido dell'Idelchik.
   begin
      return 0.5*(Csi_Attr + Csi_Espn)*Dens*Vel_In**2;
   end Perdita_Carico;
end LS.Pdc_Diff;



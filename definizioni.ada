   package LS.Numerico is
      type Mio_Float is digits 7;
      type Mio_Double is digits 14;
      type Mio_Intero is range -1E12..1E12;
   end LS.Numerico;

   with LS.Numerico; use LS.Numerico;
   with Ada.Text_IO;
   package LS.Float_IO is new Ada.Text_IO.Float_IO(Mio_Float);

   with LS.Numerico; use LS.Numerico;
   with Ada.Text_IO;
   package LS.Double_IO is new Ada.Text_IO.Float_IO(Mio_Double);

   with LS.Numerico; use LS.Numerico;
   with Ada.Text_IO;
   package LS.Integer_IO is new Ada.Text_IO.Integer_IO(Mio_Intero);

   with LS.Numerico; use LS.Numerico;
   with Ada.Numerics.Generic_Elementary_Functions;
   package LS.Funzioni_Elementari is
      new Ada.Numerics.Generic_Elementary_Functions(Mio_Float);
   with LS.Numerico; use LS.Numerico;
   with Ada.Numerics.Generic_Elementary_Functions;
   package LS.Funzioni_Elementari_Float is
      new Ada.Numerics.Generic_Elementary_Functions(Mio_Float);

   with LS.Numerico; use LS.Numerico;
   with Ada.Numerics.Generic_Elementary_Functions;
   package LS.Funzioni_Elementari_Double is
      new Ada.Numerics.Generic_Elementary_Functions(Mio_Double);


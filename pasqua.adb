with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure Pasqua is
  procedure findEaster( Anno : in integer; Mese, Giorno : out integer ) is
    -- based on the public domain algorithm
    -- by Ed Bernal
    a,b,c,e,g,h,i,k,u,x,z : integer;
  begin
    --
    --  "Gauss' famous algorithm (I don't know how or why it works,
    --  so there's no commenting)" -- Ed Bernal
    --
    a := Anno mod 19;
    b := Anno / 100;
    c := Anno rem 100;
    z := b / 4;
    e := b rem 4;
    g := (8*b + 13) / 25;
    h := (19*a + b - z - g + 15) rem 30;
    u := (a + 11*h) / 319;
    i := c / 4;
    k := c rem 4;
    x := (2*e + 2*i - k - h + u + 32) rem 7;
    Mese := (h-u+x+90) / 25;
    Giorno := (h-u+x + Mese +19) rem 32;
  end findEaster;
  Anno, Mese, Giorno : integer;
  Comando_Sbagliato : exception;
begin
   if Argument_Count < 1 then
     New_Line;
     Put("Esempio: ./pasqua 2011");
     raise Comando_Sbagliato;
  end if;
  Anno := Integer'Value(Argument(1));
  findEaster( Anno, Mese, Giorno );
  New_Line;
  Put("La Pasqua dell'anno" & Anno'Img & " è il giorno" & Giorno'Img & " di ");
  case Mese is
     when 3 => Put("Marzo");
     when 4 => Put("Aprile");
     when others => null;
  end case;
end Pasqua;


package body Julia_Set is

   procedure Calculate_Pixel (Esc         : Real;
                              Re          : Real;
                              Im          : Real;
                              Z_Escape    : out Real;
                              Iter_Escape : out Natural)
   is
      pragma Unreferenced (Esc);

      Re_Mod : Real := Re;
      Im_Mod : Real := Im;
   begin

      for I in 1 .. Max_Iterations loop
         declare
            ReS : constant Real := Re_Mod * Re_Mod;
            ImS : constant Real := Im_Mod * Im_Mod;
         begin
            if (ReS + ImS) >= Escape_Threshold then
               Iter_Escape := I;
               Z_Escape := ReS + ImS;
               return;
            end if;

            Im_Mod := (To_Real (2) * Re_Mod * Im_Mod) + Im;
            Re_Mod := (ReS - ImS) + Re;
         end;
      end loop;

      Iter_Escape := Max_Iterations;
      Z_Escape := Re_Mod * Re_Mod + Im_Mod * Im_Mod;

   end Calculate_Pixel;


end Julia_Set;

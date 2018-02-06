package body Julia_Set is



   procedure Calculate_Pixel (Self : Julia_Fractal;
                              Esc  : Complex_Type;
                              X    : ImgWidth;
                              Y    : ImgHeight;
                              Px   : out Pixel)
   is
      pragma Unreferenced (Esc);
      Z : Complex_Coordinate := Self.Get_Coordinate (X => X - 1,
                                                     Y => Y - 1);
      Zo : constant Complex_Coordinate := Z;
      N  : Color := Color'Last;

      Iters : Natural := Max_Iterations;
   begin
      for I in 1 .. Max_Iterations loop
         declare
            ReS : constant Complex_Type := Z.Re * Z.Re;
            ImS : constant Complex_Type := Z.Im * Z.Im;
         begin
            if (ReS + ImS) >= 100.0 then
               Iters := I;
               exit;
            end if;

            Z.Im := (Complex_Type (2.0 * Z.Re) * Z.Im) + Zo.Im;
            Z.Re := (ReS - ImS) + Zo.Re;
         end;
      end loop;


      Self.Calculate_Pixel_Color (Z_Mod  => (Z.Re * Z.Re + Z.Im * Z.Im),
                                  Iters  => Iters,
                                  Px     => Px);
   end Calculate_Pixel;


end Julia_Set;

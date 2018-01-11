with Ada.Streams; use Ada.Streams;

with Image_Types; use Image_Types;

package body Julia_Set is
   
   procedure Calculate_Pixel (Self : Julia_Fractal;
                              Esc  : Float;
                              X    : ImgWidth;
                              Y    : ImgHeight;
                              Px   : out Pixel)
   is
      Z : Complex := Self.Get_Coordinate (X => X - 1,
                                          Y => Y - 1);
      Zo : Complex := Z;
      N  : Color := Color'Last;
      
      Iters : Natural := Max_Iterations;
   begin
      for I in 1 .. Max_Iterations loop
         if (Z.Re * Z.Re + Z.Im * Z.Im) >= 100.0 Then
--         if abs (Z) >= 10.0 then
            Iters := I;
            exit;
         end if;
         
         Z := Z * Z + Zo;
      end loop;
      
      
      Self.Calculate_Pixel_Color (Z_Mod  => (Z.Re * Z.Re + Z.Im * Z.Im),
                                  Iters  => Iters,
                                  Px     => Px);
      end Calculate_Pixel;
         
   
end Julia_Set;

with Ada.Streams; use Ada.Streams;

with Image_Types; use Image_Types;

package body Julia_Set is 
   
   procedure Calculate_Pixel (Self : Julia_Fractal;
                              C    : Complex;
                              X    : ImgWidth;
                              Y    : ImgHeight;
                              Px   : out Pixel)
   is
      Z : Complex := Self.Get_Coordinate (X => X - 1,
                                          Y => Y - 1);
      N : Color := Color'Last;
   begin
      while abs (Z) < 10.0 and N >= 5 loop
         Z := Z * Z + C;
         N := N - 5;
      end loop;
      
      Px := Pixel'(Alpha => Color'Last,
                   others => N);
   end Calculate_Pixel;
   
end Julia_Set;

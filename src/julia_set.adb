with Ada.Numerics.Complex_Types; use Ada.Numerics.Complex_Types;


package body Julia_Set is   
   
   procedure Get_Next_Img (C_Img  : Float;
                           Width  : Natural;
                           Height : Natural;
                           Bmp    : out Pixel_Array)
   is
      Real_Range : constant R_Coords :=
                     (for I in 1 .. Width =>
                        (Float (I) * 
                         (Max_R - Min_R) / Float (Width)));
   
      Imag_Range : constant I_Coords := 
                     (for I in 1 .. Height =>
                        (Float (I) * 
                         (Max_I - Min_I) / Float (Height)));
   begin
      for X in Imag_Range'Range loop
         for Y in Real_Range'Range loop
            declare
               Z : Complex := Complex'(Re => Real_Range (Y),
                                       Im => Imag_Range (X));
               N : Color := Color'Last;
               C : Complex := Complex'(Re => 0.0,
                                       Im => C_Img);
            begin
               while abs (Z) < 10.0 and N >= 5 loop
                  Z := Z * Z + C;
                  N := N - 5;
               end loop;
               
               Bmp (X, Y) := Pixel'(Red   => N,
                                    Blue  => N,
                                    Green => N,
                                    Alpha => Color'Last);
            end;
         end loop;
      end loop;
   end Get_Next_Img;
 
end Julia_Set;

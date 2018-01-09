with Ada.Numerics.Complex_Types; use Ada.Numerics.Complex_Types;
with System;

with Image_Types; use Image_Types;

with AWS.Resources.Streams.Memory; use AWS.Resources.Streams.Memory;
with AWS.Translator; 

package body Julia_Set is 
   
   Min_R : constant := -2.0;
   Max_R : constant := 2.0;
   
   Min_I : constant := -2.0;
   Max_I : constant := 2.0;
   
   procedure Get_Next_Img (C_Img  : Float;
                           Width  : Natural;
                           Height : Natural;
                           Raw    : out Stream_Element_Array_Access)
   is
      Real_Range : constant R_Coords :=
                     (for I in 0 .. (Width - 1) =>
                        (Min_R + (Float (I) * 
                         (Max_R - Min_R) / Float (Width))));
   
      Imag_Range : constant I_Coords := 
                     (for I in 0 .. (Height - 1) =>
                        (Min_I + (Float (I) * 
                         (Max_I - Min_I) / Float (Height))));
      
      Idx : Stream_Element_Offset := Raw'First;
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
               
               Raw (Idx) := Stream_Element (N);
               Raw (Idx + 1) := Stream_Element (N);
               Raw (Idx + 2) := Stream_Element (N);
               Raw (Idx + 3) := Stream_Element (Color'Last);
               
               Idx := Idx + Pixel'Size / 8;                 
            end;
         end loop;
      end loop;
   end Get_Next_Img;
 
end Julia_Set;

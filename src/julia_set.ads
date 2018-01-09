with Ada.Streams; use Ada.Streams;

with AWS.Utils; use AWS.Utils;

package Julia_Set is   
   
   procedure Get_Next_Img (C_Img : Float;
                           Width : Natural;
                           Height : Natural;
                           Raw   : out Stream_Element_Array_Access);
 
private   
   type I_Coords is array (Natural range <>) of Float;
   type R_Coords is array (Natural range <>) of Float;
   
end Julia_Set;

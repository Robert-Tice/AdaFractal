with Image_Types; use Image_Types;

package Julia_Set is   
   
   procedure Get_Next_Img (C_Img : Float;
                           Width : Natural;
                           Height : Natural;
                           Bmp   : out Pixel_Array);
 
private   
   type I_Coords is array (Natural range <>) of Float;
   type R_Coords is array (Natural range <>) of Float;
   
end Julia_Set;

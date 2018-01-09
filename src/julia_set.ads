with Image_Types; use Image_Types;

generic
   Min_R, Max_R : Float;
   Min_I, Max_I : Float;
   Img_Width : Natural;
   Img_Height : Natural;
package Julia_Set is   
   
   procedure Get_Next_Img (C_Img : Float;
                           Width : Natural;
                           Height : Natural;
                           Bmp   : out Pixel_Array);
 
private
   
   subtype W_Type is Integer range 1 .. Img_Width;
   subtype H_Type is Integer range 1 .. Img_Height;
   
   subtype R_Type is Float range Min_R .. Max_R;
   subtype I_Type is Float range Min_I .. Max_I;
   
   type I_Coords is array (Natural range <>) of Float;
   type R_Coords is array (Natural range <>) of Float;
   
end Julia_Set;

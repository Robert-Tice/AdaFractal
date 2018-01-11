with Ada.Numerics.Complex_Types; use Ada.Numerics.Complex_Types;
with Ada.Streams; use Ada.Streams;

with AWS.Utils; use AWS.Utils;

with Fractal; use Fractal;
with Image_Types; use Image_Types;

package Julia_Set is
   
   type Julia_Fractal is new Abstract_Fractal with null record;
   
private
   
   procedure Calculate_Pixel (Self : Julia_Fractal; 
                              Esc  : Float;
                              X    : ImgWidth;
                              Y    : ImgHeight;
                              Px   : out Pixel);
   
   
end Julia_Set;

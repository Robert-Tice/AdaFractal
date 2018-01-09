with Ada.Streams; use Ada.Streams;

package Image_Types is

   type Color is new Natural range 0 .. 255
     with Size => 8;
   
   type Pixel is record
      Red   : Color;
      Green : Color;
      Blue  : Color;
      Alpha : Color;
   end record;
   
   for Pixel use record
      Red at 0 range 0 .. 7;
      Green at 1 range 0 .. 7;
      Blue at 2 range 0 .. 7;
      Alpha at 3 range 0 .. 7;
   end record;
   
   type Pixel_Array is array 
     (Natural range <>, 
      Natural range <>) of Pixel
     with Pack;
      

end Image_Types;

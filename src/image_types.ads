package Image_Types is

   subtype ImgWidth is Positive range 1 .. 1920;
   subtype ImgHeight is Positive range 1 .. 1080;
   subtype ImgZoom is Positive range 1 .. 1000;

   type Coordinate is record
      X : ImgWidth;
      Y : ImgHeight;
   end record;

   Pixel_Size : constant := 4;  --  bytes

   type Color is new Natural range 0 .. 255
     with Size => 8;

   Max_Iterations : constant := Color'Last / 5;

   type Pixel is record
      Red   : Color;
      Green : Color;
      Blue  : Color;
      Alpha : Color;
   end record
     with Size => 32;

   for Pixel use record
      Red at 0 range 0 .. 7;
      Green at 1 range 0 .. 7;
      Blue at 2 range 0 .. 7;
      Alpha at 3 range 0 .. 7;
   end record;

   type Pixel_Array is array
     (Natural range <>) of Pixel
     with Pack;

   type Pixel_Array_Ptr is access all Pixel_Array;


end Image_Types;

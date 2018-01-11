with Ada.Streams; use Ada.Streams;

with AWS.Response;
with AWS.Status;
with AWS.Utils; use AWS.Utils;

with Image_Types; use Image_Types;

with Julia_Set; use Julia_Set;

package Router_Cb is
   function Router (Request : AWS.Status.Data) return AWS.Response.Data;
   
   procedure Initialize_Fractals;
   
   Server_Alive  : Boolean := True;
   
   Max_Buffer_Size : constant := 
                       ImgWidth'Last * ImgHeight'Last * (Pixel_Size);
   
   RawData : Stream_Element_Array_Access := 
               new Stream_Element_Array (1 .. Max_Buffer_Size);
   
private
   
   type Counter is mod 10;
   
   Frame_Counter : Counter := 0; 
   
   procedure ImgSize_Parse (URI : String);
   
   JFractal : aliased Julia_Fractal;

end Router_Cb;

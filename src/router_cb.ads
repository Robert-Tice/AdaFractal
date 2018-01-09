with Ada.Streams; use Ada.Streams;

with AWS.Response;
with AWS.Status;
with AWS.Utils; use AWS.Utils;

with Image_Types; use Image_Types;

package Router_Cb is
   function Router (Request : AWS.Status.Data) return AWS.Response.Data;
   
   Server_Alive  : Boolean := True;
   
   Max_Width : constant := 1920;
   Max_Height : constant := 1080;
   
   RawData : Stream_Element_Array_Access := 
               new Stream_Element_Array (1 .. Max_Width * Max_Height * (Pixel'Size / 8));
   
private
   
   type Counter is mod 100;
   
   Frame_Counter : Counter := 0; 
   
   function Fractal_Parse (URI : String) return Stream_Element_Offset;

end Router_Cb;

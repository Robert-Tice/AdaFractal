
with AWS.Response;
with AWS.Status;

with Image_Types; use Image_Types;

package Router_Cb is
   function Router (Request : AWS.Status.Data) return AWS.Response.Data;
   
   Server_Alive  : Boolean := True;
private
   
   type Counter is mod 10;
   
   Frame_Counter : Counter := 0;
   
   function Get_Img (Width  : Natural;
                     Height : Natural)
                     return AWS.Response.Data;

end Router_Cb;

with Julia_Set; use Julia_Set;
with Image_Types; use Image_Types;

with Ada.Calendar; use Ada.Calendar;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics.Complex_Types; use Ada.Numerics.Complex_Types;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

with AWS.MIME;
with AWS.Messages;
with AWS.Resources.Streams.Memory;
with AWS.Utils;

with Fractal;

package body Router_Cb is
   
   procedure Initialize_Fractals
   is
   begin
      Fractal.Initialize (Frct_Ptr => JFractal'Access);
   end Initialize_Fractals;
   
   function Router (Request : AWS.Status.Data) return AWS.Response.Data
   is      
      URI      : constant String := AWS.Status.URI (Request);
      Filename : constant String := "web/" & URI (2 .. URI'Last);
   begin
      Put_Line ("URI: " & URI);
      
      if URI = "/" then
         --  main page
         return AWS.Response.File (AWS.MIME.Text_HTML, "web/html/index.html");
      elsif URI = "/fractal" then
         --  get new image and send it back to client 
         
         declare            
            Start_Time : Time := Clock;
            Diff_Time  : Duration;
         begin
            
            JFractal.Calculate_Image (Esc    => Float (JFractal.Get_Frame),
                                      Buffer => RawData);

            Diff_Time := Clock - Start_Time;
            
            Put_Line ("Time:" & Duration'Image (Diff_Time));            
            
            return AWS.Response.Build (Content_Type  => AWS.MIME.Application_Octet_Stream,
                                       Message_Body  => RawData 
                                         (RawData'First .. 
                                            RawData'First + JFractal.Get_Buffer_Size));
         end;
      elsif URI = "/quit" then
         Router_Cb.Server_Alive := False;
         Put_Line ("quitting...");
         return AWS.Response.Build (AWS.MIME.Text_HTML, "quitting...");
      elsif URI'Length > 8 and then 
        URI (URI'First .. URI'First + 7) = "/window|" then
         ImgSize_Parse (URI => URI (URI'First + 8 .. URI'Last));
         
         return AWS.Response.Build (AWS.MIME.Text_HTML, "Success");
      elsif AWS.Utils.Is_Regular_File (Filename) then
         return AWS.Response.File
           (Content_Type => AWS.MIME.Content_Type (Filename),
            Filename     => Filename);
      else
         Put_Line ("Could not find file: " & Filename);
         
         return AWS.Response.Acknowledge
           (AWS.Messages.S404,
            "<p>Page '" & URI & "' Not found.");
      end if;

   end Router;
   
   procedure ImgSize_Parse (URI : String)
   is            
      Width, Height : Natural;
      QPos   : Integer := (-1);
      
      Invalid_Get : exception;
   begin
      Put_Line ("RawStr: " & URI);
            
      for I in URI'Range loop
         if URI (I) = '|' then
            QPos := I;
            exit;
         end if;
      end loop;
            
      if QPos = (-1) then
         raise Invalid_Get;
      end if;
                  
      Width := Natural'Value (URI (URI'First .. QPos - 1));
      Height := Natural'Value (URI (QPos + 1 .. URI'Last));
            
      if Width > ImgWidth'Last then
         Width := ImgWidth'Last;
      end if;
      if Height > ImgHeight'Last then
         Height := ImgHeight'Last;
      end if;
            
      JFractal.Set_Size (Width  => Width,
                         Height => Height);
            
      Put_Line ("Width:" & Width'Img & " Height:" & Height'Img);
           
   end ImgSize_Parse;      

end Router_Cb;

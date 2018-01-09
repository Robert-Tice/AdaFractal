with Julia_Set; use Julia_Set;
with Image_Types; use Image_Types;

with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

with AWS.MIME;
with AWS.Messages;
with AWS.Resources.Streams.Memory;
with AWS.Utils;

with System;

package body Router_Cb is
   
   function Router (Request : AWS.Status.Data) return AWS.Response.Data
   is      
      URI      : constant String := AWS.Status.URI (Request);
      Filename : constant String := "web/" & URI (2 .. URI'Last);
   begin
--      Put_Line ("URI: " & URI);
      
      if URI = "/" then
         --  main page
         return AWS.Response.File (AWS.MIME.Text_HTML, "web/html/index.html");
      elsif URI'Length > 9 and then
        URI (URI'First .. URI'First + 8) = "/fractal|" then
         --  get new image and send it back to client 
         declare
            RawStr : constant String := URI (URI'First + 9 .. URI'Last);
            
            Width, Height : Natural;
            QPos   : Integer := (-1);
         begin
 --           Put_Line ("RawStr: " & RawStr);
            
            for I in RawStr'Range loop
               if RawStr (I) = '|' then
                  QPos := I;
                  exit;
               end if;
            end loop;
            
            if QPos = (-1) then
               return AWS.Response.Acknowledge
                 (AWS.Messages.S404,
                  "<p>Page '" & URI & "' Not found.");
            end if;
                  
            Width := Natural'Value (RawStr (RawStr'First .. QPos - 1));
            Height := Natural'Value (RawStr (QPos + 1 .. RawStr'Last));
            
--            Put_Line ("Width:" & Width'Img & " Height:" & Height'Img);
               
            return Get_Img (Width  => Width,
                            Height => Height);
         end;
      elsif URI = "/quit" then
         Router_Cb.Server_Alive := False;
         Put_Line ("quitting...");
         return AWS.Response.File (AWS.MIME.Text_Plain, "quitting...");
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
   
   
   function Get_Img (Width  : Natural;
                     Height : Natural) 
                     return AWS.Response.Data
   is            
      Img : Pixel_Array (1 .. Width, 1 .. Height);
      Buf : Stream_Element_Array (1 .. Img'Size / 8)
        with Address => Img'Address;
      
   begin
      Get_Next_Img (C_Img  => Float (Frame_Counter) / 10.0,
                    Width  => Width,
                    Height => Height,
                    Bmp    => Img);
      Frame_Counter := Frame_Counter + 1;
     
      return AWS.Response.Build (Content_Type  => AWS.MIME.Application_Octet_Stream,
                                 Message_Body  => Buf);
   end Get_Img;

end Router_Cb;

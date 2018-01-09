with Julia_Set; use Julia_Set;
with Image_Types; use Image_Types;

with Ada.Calendar; use Ada.Calendar;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

with AWS.MIME;
with AWS.Messages;
with AWS.Resources.Streams.Memory;
with AWS.Utils;

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
            BufferSize : Stream_Element_Offset;
            
            Start_Time : Time := Clock;
            Diff_Time  : Duration;
         begin
            BufferSize := Fractal_Parse (URI => URI (URI'First + 9 .. URI'Last));

            Diff_Time := Clock - Start_Time;
            
            Put_Line ("Time:" & Duration'Image (Diff_Time));
            return AWS.Response.Build (Content_Type  => AWS.MIME.Application_Octet_Stream,
                                       Message_Body  => RawData (1 .. BufferSize));
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
   
   function Fractal_Parse (URI : String) return Stream_Element_Offset
   is            
      Width, Height : Natural;
      QPos   : Integer := (-1);
            
      BufferSize    : Stream_Element_Offset;
      
      Invalid_Get : exception;
   begin
      --           Put_Line ("RawStr: " & RawStr);
            
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
            
      if Width > Max_Width then
         Width := Max_Width;
      end if;
      if Height > Max_Height then
         Height := Max_Height;
      end if;
            
      BufferSize := Stream_Element_Offset (Width) *
        Stream_Element_Offset (Height) *
        Stream_Element_Offset (Pixel'Size / 8);
            
      --            Put_Line ("Width:" & Width'Img & " Height:" & Height'Img);
            
           
      Get_Next_Img (C_Img  => Float (Frame_Counter) / 100.0,
                    Width  => Width,
                    Height => Height,
                    Raw    => RawData);
      Frame_Counter := Frame_Counter + 1;
      
      return BufferSize;
   end Fractal_Parse;
      

end Router_Cb;

with Julia_Set;
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
      if URI = "/" then
         --  main page
         return AWS.Response.File (AWS.MIME.Text_HTML, "web/html/index.html");
      elsif URI = "/fractal" then
         --  get new image and send it back to client      
         return Get_Img (Width  => Canvas_Width,
                         Height => Canvas_Height);
      elsif URI = "/quit" then
         Router_Cb.Server_Alive := False;
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
   
   procedure Print_Img (Buf : Stream_Element_Array)
   is
      Col : Natural := 1;
      
      Idx : Ada.Streams.Stream_Element_Offset := Buf'First;
   begin
      while Idx <= Buf'Last loop
         if Col > Canvas_Width then
            New_Line;
            Col := 1;
         end if;
         
         Ada.Text_IO.Put (Buf (Idx)'Img);
         Col := Col + 1;
         Idx := Idx + 4;
      end loop;
      
      New_Line;
   end Print_Img;
   
   function Get_Img (Width : Natural;
                     Height : Natural) 
                     return AWS.Response.Data
   is      
      package Fractal is new Julia_Set (Min_R      => -2.0,
                                        Max_R      => 2.0,
                                     Min_I      => -2.0,
                                     Max_I      => 2.0,
                                     Img_Width  => Width,
                                     Img_Height => Height);
      use Fractal;
      
      Img : Pixel_Array (1 .. Width, 1 .. Height);
      Buf : Stream_Element_Array (1 .. Img'Size / 8)
        with Address => Img'Address;
      
   begin
      Get_Next_Img (C_Img => Float (Frame_Counter) / 10.0,
                    Width => Width,
                    Height => Height,
                    Bmp   => Img);
      Frame_Counter := Frame_Counter + 1;
      
 --     Print_Img (Buf => Buf);
      return AWS.Response.Build (Content_Type  => AWS.MIME.Application_Octet_Stream,
                                 Message_Body  => Buf);
   end Get_Img;

end Router_Cb;

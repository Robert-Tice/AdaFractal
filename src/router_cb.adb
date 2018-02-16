with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;

with AWS.MIME;
with AWS.Messages;

with Fractal;

package body Router_Cb is
   
   procedure Init
   is
   begin
      Float_Julia_Fractal.Init (Viewport => Viewport);
      Fixed_Julia_Fractal.Init (Viewport => Viewport);
   end Init;

   function Router (Request : AWS.Status.Data) return AWS.Response.Data
   is
      URI      : constant String := AWS.Status.URI (Request);
      Filename : constant String := "web/" & URI (2 .. URI'Last);
      
      Buffer_Size : Stream_Element_Offset;
   begin
--      Put_Line ("URI: " & URI);
      
      --  Main server access point
      if URI = "/" then
         --  main page
         return AWS.Response.File (AWS.MIME.Text_HTML, "web/html/index.html");
         
      --  Requests a new image from the server with fixed point numbers
      elsif URI = "/fixed_fractal" then       
         Buffer_Size := Compute_Image (Comp_Type => Fixed_Type);
         
         return AWS.Response.Build
           (Content_Type  => AWS.MIME.Application_Octet_Stream,
            Message_Body  => RawData (RawData'First ..
                  RawData'First + Buffer_Size));
      --  Requests a new image from the server with floating point numbers
      elsif URI = "/float_fractal" then
         Buffer_Size := Compute_Image (Comp_Type => Float_Type);
         
         return AWS.Response.Build
           (Content_Type  => AWS.MIME.Application_Octet_Stream,
            Message_Body  => RawData (RawData'First ..
                  RawData'First + Buffer_Size));
         
      --  Resets the viewport to the default view
      elsif URI = "/reset" then
         Reset_Viewport;
         return AWS.Response.Build (AWS.MIME.Text_HTML, "reset");        
         
      --  Quits the application and kills the server
      elsif URI = "/quit" then
         Router_Cb.Server_Alive := False;
         Put_Line ("quitting...");
         return AWS.Response.Build (AWS.MIME.Text_HTML, "quitting...");
         
      --  Requests last compute time
      elsif URI = "/compute_time" then
         return AWS.Response.Build 
           (AWS.MIME.Text_HTML, Duration'Image (Compute_Time));
         
      --  Looking for window size information
      elsif URI'Length > 8 and then
        URI (URI'First .. URI'First + 7) = "/window|"
      then
         ImgSize_Parse (URI => URI (URI'First + 8 .. URI'Last));

         return AWS.Response.Build (AWS.MIME.Text_HTML, "Success");
         
      --  Serve basic files
      elsif AWS.Utils.Is_Regular_File (Filename) then
         return AWS.Response.File
           (Content_Type => AWS.MIME.Content_Type (Filename),
            Filename     => Filename);
         
      --  404 not found
      else
         Put_Line ("Could not find file: " & Filename);

         return AWS.Response.Acknowledge
           (AWS.Messages.S404,
            "<p>Page '" & URI & "' Not found.");
      end if;

   end Router;

   procedure ImgSize_Parse (URI : String)
   is
      Width, Height, Zoom, MouseX, MouseY : Natural;
      Seps                : array (Natural range 1 .. 4) of Integer := 
                              (others => (-1));
      Sep_Counter         : Natural := Seps'First;

      Invalid_Get : exception;
   begin
--      Put_Line ("RawStr: " & URI);         

      --  find position of all separators
      for I in URI'Range loop
         if URI (I) = '|' then
            Seps (Sep_Counter) := I;
            if Sep_Counter = Seps'Last then
               exit;
            end if;
            
            Sep_Counter := Sep_Counter + 1;
         end if;
      end loop;
      
      if Sep_Counter /= Seps'Last then
         raise Invalid_Get;
      end if;

      Width := Natural'Value (URI (URI'First .. Seps (1) - 1));
      Height := Natural'Value (URI (Seps (1) + 1 .. Seps (2) - 1));
      Zoom := Natural'Value (URI (Seps (2) + 1 .. Seps (3) - 1));
      MouseX := Natural'Value (URI (Seps (3) + 1 .. Seps (4) - 1));
      MouseY := Natural'Value (URI (Seps (4) + 1 .. URI'Last));
      
      if Width >= 0 then
         if Width > ImgWidth'Last then
            Width := ImgWidth'Last;
         end if;
         
         Viewport.Width := Width;
      end if;
      
      if Height >= 0 then 
         if Height > ImgHeight'Last then
            Height := ImgHeight'Last;
         end if;
         
         Viewport.Height := Height;
      end if;
      
      if Zoom /= 0 then
         Zoom := Viewport.Zoom + Zoom;
         
         if Zoom > ImgZoom'Last then
            Zoom := ImgZoom'Last;
         elsif Zoom < ImgZoom'First then
            Zoom := ImgZoom'First;
         end if;
         
         Viewport.Zoom := Zoom;
      end if;
      
      if MouseX >= 0 then 
         if MouseX > ImgWidth'Last then
            MouseX := ImgWidth'Last;
         elsif MouseX < ImgWidth'First then
            MouseX := ImgWidth'First;
         end if;
         
         Viewport.Center.X := MouseX;
      end if;
      
      if MouseY >= 0 then
         if MouseY > ImgHeight'Last then
            MouseY := ImgHeight'Last;
         elsif MouseY < ImgHeight'First then
            MouseY := ImgHeight'First;
         end if;
         
         Viewport.Center.Y := MouseY;
      end if;

      Put_Line ("Float");
      Float_Julia_Fractal.Set_Size (Viewport => Viewport);
      
      Put_Line ("Fixed");
      Fixed_Julia_Fractal.Set_Size (Viewport => Viewport);

      Put_Line ("Width:" & Viewport.Width'Img & 
                  " Height:" & Viewport.Height'Img & 
                  " Zoom:" & Viewport.Zoom'Img & 
                  " MouseX:" & Viewport.Center.X'Img &
                  " MouseY:" & Viewport.Center.Y'Img);

   end ImgSize_Parse;
   
   function Compute_Image (Comp_Type : Computation_Enum) 
                           return Stream_Element_Offset
   is
      Start_Time : constant Time := Clock;
      Ret : Stream_Element_Offset;
   begin
      case Comp_Type is
         when Fixed_Type =>
            Fixed_Julia_Fractal.Calculate_Image 
              (Esc    => Real_Fixed (Fixed_Julia_Fractal.Get_Frame),
               Buffer => RawData);
            Ret := Fixed_Julia_Fractal.Get_Buffer_Size;
         when Float_Type =>
            Float_Julia_Fractal.Calculate_Image 
              (Esc    => Real_Float (Float_Julia_Fractal.Get_Frame),
               Buffer => RawData);
            Ret := Float_Julia_Fractal.Get_Buffer_Size;
      end case;
      
      Compute_Time := (Clock - Start_Time) * 1000.0; 
      
--      Put_Line ("Time:" & Duration'Image (Compute_Time) & " ms");
       
      return Ret;
   end Compute_Image;
   
   procedure Reset_Viewport
   is
   begin
      Viewport.Zoom := 10;
      Viewport.Center.X := Viewport.Width / 2;
      Viewport.Center.Y := Viewport.Height / 2;
      
      Float_Julia_Fractal.Set_Size (Viewport => Viewport);
      
      Fixed_Julia_Fractal.Set_Size (Viewport => Viewport);
      
      Put_Line ("Width:" & Viewport.Width'Img & 
                  " Height:" & Viewport.Height'Img & 
                  " Zoom:" & Viewport.Zoom'Img & 
                  " MouseX:" & Viewport.Center.X'Img &
                  " MouseY:" & Viewport.Center.Y'Img);
      
   end Reset_Viewport;
   
end Router_Cb;

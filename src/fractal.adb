with Ada.Exceptions;

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib;

package body Fractal is

   procedure Init (Viewport : Viewport_Info)
   is
   begin
      S_Width := Viewport.Width;
      S_Height := Viewport.Height;
      S_Zoom := Viewport.Zoom;

      S_Center := Complex_Coordinate'(Re => To_Real (0),
                                      Im => To_Real (0));

      Calculate_Bounds;
      Calculate_Step;
   end Init;

   procedure Set_Size (Viewport : Viewport_Info)
   is
   begin
      S_Width := Viewport.Width;
      S_Height := Viewport.Height;
      S_Zoom := Viewport.Zoom;


      S_Center := Get_Coordinate (Coord => Viewport.Center);

      Calculate_Bounds;
      Calculate_Step;

      Put_Line ("Re:" & Image (S_Real_Min) & "," & Image (S_Real_Max) &
                  " Im:" & Image (S_Imag_Min) & "," & Image (S_Imag_Max));
      Put_Line ("Center:" & Image (S_Center.Re) & "," & Image (S_Center.Im));
   end Set_Size;

   function Get_Coordinate (X : ImgWidth;
                            Y : ImgHeight)
                            return Complex_Coordinate
   is
      Real_Coord : constant Real := S_Real_Min + F_To_Real (Float (X) * To_Float (S_Real_Step));
      Imag_Coord : constant Real := S_Imag_Min + F_To_Real (Float (Y) * To_Float (S_Imag_Step));
   begin
      return Complex_Coordinate'(Re => Real_Coord,
                                 Im => Imag_Coord);
   end Get_Coordinate;

   procedure Calculate_Bounds
   is
      Half_Real_Dist, Half_Imag_Dist : Real;

      Inv_Aspect_Ratio               : constant Real := F_To_Real(
                                         Float (S_Height) /
                                         Float (S_Width));
   begin
      Half_Real_Dist := Real_Distance_Unzoomed * To_Real (10) /
        (To_Real (S_Zoom) * To_Real (2));
      Half_Imag_Dist := Inv_Aspect_Ratio * Half_Real_Dist;

      S_Real_Max := Half_Real_Dist + S_Center.Re;
      S_Real_Min := S_Center.Re - Half_Real_Dist;

      if S_Real_Max > Real_Distance_Unzoomed / To_Real (2) then
         S_Real_Max := Real_Distance_Unzoomed / To_Real (2);
      end if;
      if S_Real_Min < Real_Distance_Unzoomed / To_Real (-2) then
         S_Real_Min := Real_Distance_Unzoomed / To_Real (-2);
      end if;

      S_Imag_Max := Half_Imag_Dist + S_Center.Im;
      S_Imag_Min := S_Center.Im - Half_Imag_Dist;

      if S_Imag_Max > Real_Distance_Unzoomed * Inv_Aspect_Ratio / To_Real (2) then
         S_Imag_Max := Real_Distance_Unzoomed * Inv_Aspect_Ratio / To_Real (2);
      end if;
      if S_Imag_Min < Real_Distance_Unzoomed * Inv_Aspect_Ratio / To_Real (-2) then
         S_Imag_Min := Real_Distance_Unzoomed * Inv_Aspect_Ratio / To_Real(-2);
      end if;
   end Calculate_Bounds;

   procedure Calculate_Step
   is
   begin
      S_Real_Step := F_To_Real (To_Float (S_Real_Max - S_Real_Min) / Float (S_Width));
      S_Imag_Step := F_To_Real (To_Float (S_Imag_Max - S_Imag_Min) / Float (S_Height));
   end Calculate_Step;

   procedure Calculate_Image (Esc    : Real;
                              Buffer : out Stream_Element_Array_Access)
   is
      Chunk_Size : constant Natural := S_Height / Task_Pool_Size;
   begin

      for I in S_Task_Pool'First .. S_Task_Pool'Last - 1 loop
         S_Task_Pool (I).Go (Start_Row => ImgHeight'First +
                                (I - S_Task_Pool'First) * Chunk_Size,
                                Stop_Row  => I * Chunk_Size,
                                E         => Esc,
                                Buf       => Buffer);
      end loop;
      S_Task_Pool (S_Task_Pool'Last).Go
        (Start_Row => ImgHeight'First +
           (S_Task_Pool'Last - S_Task_Pool'First) * Chunk_Size,
         Stop_Row  => S_Height,
         E         => Esc,
         Buf       => Buffer);

      S_Sync_Obj.Wait_For_Complete;

   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
         GNAT.OS_Lib.OS_Exit (-1);
   end Calculate_Image;

   procedure Calculate_Pixel_Color (Z_Escape     : Real;
                                    Iter_Escape  : Natural;
                                    Px           : out Pixel)
   is
      Value : constant Integer := 765 * (Iter_Escape - 1) / Max_Iterations;
   begin
      if Z_Escape > To_Real (4) then
         if Value > 510 then
            Px := Pixel'(Red   => Color'Last - S_Frame_Counter,
                         Green => Color'Last,
                         Blue  => Color (Value rem Integer (Color'Last)),
                         Alpha => Color'Last);
         elsif Value > 255 then
            Px := Pixel'(Red   => Color'Last - S_Frame_Counter,
                         Green => Color (Value rem Integer (Color'Last)),
                         Blue  => Color'First + S_Frame_Counter,
                         Alpha => Color'Last);
         else
            Px := Pixel'(Red   => Color (Value rem Integer (Color'Last)),
                         Green => Color'First + S_Frame_Counter,
                         Blue  => Color'First,
                         Alpha => Color'Last);
         end if;
      else
         Px := Pixel'(Red   => Color'First + S_Frame_Counter,
                      Green => Color'First + S_Frame_Counter,
                      Blue  => Color'First + S_Frame_Counter,
                      Alpha => Color'Last);
      end if;

   end Calculate_Pixel_Color;

   function Get_Frame return Color
   is
   begin
      if S_Cnt_Up then
         if S_Frame_Counter = Color'Last then
            S_Cnt_Up := not S_Cnt_Up;
            return S_Frame_Counter;
         else
            S_Frame_Counter := S_Frame_Counter + 5;
            return (S_Frame_Counter - 5);
         end if;
      end if;

      if S_Frame_Counter = Color'First then
         S_Cnt_Up := not S_Cnt_Up;
         return S_Frame_Counter;
      end if;

      S_Frame_Counter := S_Frame_Counter - 5;
      return (S_Frame_Counter + 5);

   end Get_Frame;

   procedure Calculate_Row (Esc  : Real;
                            Y    : ImgHeight;
                            Idx  : Stream_Element_Offset;
                            Buffer : out not null Stream_Element_Array_Access)
   is
      Line : Pixel_Array (1 .. Get_Width)
        with Address => Buffer (Idx)'Address;

      Coord : Complex_Coordinate;

      Z_Esc : Real;
      I_Esc : Natural;
   begin
      for X in Line'Range loop
         Coord := Get_Coordinate (X => X,
                                  Y => Y);
         Calculate_Pixel (Esc         => Esc,
                          Re          => Coord.Re,
                          Im          => Coord.Im,
                          Z_Escape    => Z_Esc,
                          Iter_Escape => I_Esc);
         Calculate_Pixel_Color (Z_Escape    => Z_Esc,
                                Iter_Escape => I_Esc,
                                Px          => Line (X));
      end loop;
   end Calculate_Row;

   protected body Pool_Sync is

      procedure Finished
      is
      begin
         Complete_Tasks := Complete_Tasks + 1;
      end Finished;

      entry Wait_For_Complete when Complete_Tasks = Task_Pool_Size is
      begin
         Complete_Tasks := 0;
      end Wait_For_Complete;

   end Pool_Sync;

   task body Chunk_Task
   is
      Start   : Natural;
      Stop    : Natural;
      Esc     : Real;
      Buffer  : Stream_Element_Array_Access;
   begin

      loop
         accept Go (Start_Row : Natural;
                    Stop_Row  : Natural;
                    E         : Real;
                    Buf       : Stream_Element_Array_Access) do
            Start := Start_Row;
            Stop := Stop_Row;
            Esc := E;
            Buffer := Buf;
         end Go;

         for I in Start .. Stop loop

            Calculate_Row (Esc    => Esc,
                           Y      => I,
                           Idx    => Buffer'First +
                             Stream_Element_Offset ((I - 1) *
                                   Get_Width * Pixel'Size / 8),
                           Buffer => Buffer);
         end loop;
         S_Sync_Obj.Finished;
      end loop;
   end Chunk_Task;

end Fractal;

with Ada.Exceptions;

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib;

package body Fractal is

   procedure Initialize (Frct_Ptr : Abstract_Fractal_Ptr)
   is
   begin

      for I in Frct_Ptr.Task_Pool'Range loop
         Frct_Ptr.Task_Pool (I).Initialize (F => Frct_Ptr);

      end loop;

      Frct_Ptr.Set_Size (Width  => ImgWidth'Last,
                         Height => ImgHeight'Last);

   end Initialize;

   procedure Set_Size (Self   : in out Abstract_Fractal;
                       Width  : ImgWidth;
                       Height : ImgHeight)
   is
   begin
      Self.Width := Width;
      Self.Height := Height;

      Self.Calculate_Imaginary_Bounds;
      Self.Calculate_Step;

   end Set_Size;

   function Get_Coordinate (Self : Abstract_Fractal;
                            X    : Natural;
                            Y    : Natural)
                            return Complex_Coordinate
   is
      Real_Coord : constant Complex_Type :=
        Real_Min + (Complex_Type (X) * Self.Real_Step);

      Imag_Coord : constant Complex_Type :=
        Self.Imag_Min + (Complex_Type (Y) * Self.Imag_Step);
   begin
      return Complex_Coordinate'(Re => Real_Coord,
                                 Im => Imag_Coord);
   end Get_Coordinate;

   procedure Calculate_Imaginary_Bounds (Self : in out Abstract_Fractal)
   is
      Inv_Aspect_Ratio : constant Complex_Type := Complex_Type (Self.Height) /
        Complex_Type (Self.Width);
   begin
      Self.Imag_Max := Inv_Aspect_Ratio *
        Complex_Type ((Real_Max - Real_Min) / 2.0);

      Self.Imag_Min := (-1.0) * Self.Imag_Max;
   end Calculate_Imaginary_Bounds;

   procedure Calculate_Step (Self : in out Abstract_Fractal)
   is
   begin
      Self.Real_Step := (Real_Max - Real_Min) / Complex_Type (Self.Width);
      Self.Imag_Step := (Self.Imag_Max - Self.Imag_Min) / Complex_Type (Self.Height);
   end Calculate_Step;

   procedure Calculate_Image (Self : in out Abstract_Fractal;
                              Esc  : Complex_Type;
                              Buffer : out Stream_Element_Array_Access)
   is
      Chunk_Size : constant Natural := Self.Height / Task_Pool_Size;
   begin

      for I in Self.Task_Pool'First .. Self.Task_Pool'Last - 1 loop
         Self.Task_Pool (I).Go (Start_Row => ImgHeight'First +
                                (I - Self.Task_Pool'First) * Chunk_Size,
                                Stop_Row  => I * Chunk_Size,
                                E         => Esc,
                                Buf       => Buffer);
      end loop;
      Self.Task_Pool (Self.Task_Pool'Last).Go
        (Start_Row => ImgHeight'First +
           (Self.Task_Pool'Last - Self.Task_Pool'First) * Chunk_Size,
         Stop_Row  => Self.Height,
         E         => Esc,
         Buf       => Buffer);

      Self.Sync_Obj.Wait_For_Complete;

   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
         GNAT.OS_Lib.OS_Exit (-1);
   end Calculate_Image;

   procedure Calculate_Pixel_Color (Self  : Abstract_Fractal;
                                    Z_Mod : Complex_Type;
                                    Iters  : Natural;
                                    Px    : out Pixel)
   is
      Value : constant Integer := 765 * (Iters - 1) / Max_Iterations;
   begin
      if Z_Mod > 4.0 then
         if Value > 510 then
            Px := Pixel'(Red   => Color'Last - Self.Frame_Counter,
                         Green => Color'Last,
                         Blue  => Color (Value rem Integer (Color'Last)),
                         Alpha => Color'Last);
         elsif Value > 255 then
            Px := Pixel'(Red   => Color'Last - Self.Frame_Counter,
                         Green => Color (Value rem Integer (Color'Last)),
                         Blue  => Color'First + Self.Frame_Counter,
                         Alpha => Color'Last);
         else
            Px := Pixel'(Red   => Color (Value rem Integer (Color'Last)),
                         Green => Color'First + Self.Frame_Counter,
                         Blue  => Color'First,
                         Alpha => Color'Last);
         end if;
      else
         Px := Pixel'(Red   => Color'First + Self.Frame_Counter,
                      Green => Color'First + Self.Frame_Counter,
                      Blue  => Color'First + Self.Frame_Counter,
                      Alpha => Color'Last);
      end if;

   end Calculate_Pixel_Color;

   function Get_Frame (Self : in out Abstract_Fractal) return Color
   is
   begin
      if Self.Cnt_Up then
         if Self.Frame_Counter = Color'Last then
            Self.Cnt_Up := not Self.Cnt_Up;
            return Self.Frame_Counter;
         else
            Self.Frame_Counter := Self.Frame_Counter + 5;
            return (Self.Frame_Counter - 5);
         end if;
      end if;

      if Self.Frame_Counter = Color'First then
         Self.Cnt_Up := not Self.Cnt_Up;
         return Self.Frame_Counter;
      end if;

      Self.Frame_Counter := Self.Frame_Counter - 5;
      return (Self.Frame_Counter + 5);

   end Get_Frame;

   procedure Calculate_Row (Self : Abstract_Fractal;
                            Esc  : Complex_Type;
                            Y    : ImgHeight;
                            Idx  : Stream_Element_Offset;
                            Buffer : out not null Stream_Element_Array_Access)
   is
      Line : Pixel_Array (1 .. Self.Get_Width)
        with Address => Buffer (Idx)'Address;
   begin
      for X in Line'Range loop
         Abstract_Fractal'Class (Self).Calculate_Pixel (Esc  => Esc,
                                                        X    => X,
                                                        Y    => Y,
                                                        Px   => Line (X));
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
      Fct     : Abstract_Fractal_Ptr;
      Esc     : Complex_Type;
      Buffer  : Stream_Element_Array_Access;
   begin
      accept Initialize (F : Abstract_Fractal_Ptr) do
         Fct := F;
      end Initialize;

      loop
         accept Go (Start_Row : Natural;
                    Stop_Row  : Natural;
                    E         : Complex_Type;
                    Buf       : Stream_Element_Array_Access) do
            Start := Start_Row;
            Stop := Stop_Row;
            Esc := E;
            Buffer := Buf;
         end Go;

         for I in Start .. Stop loop

            Fct.Calculate_Row (Esc  => Esc,
                               Y    => I,
                               Idx  => Buffer'First +
                                 Stream_Element_Offset ((I - 1) *
                                       Fct.Get_Width * Pixel'Size / 8),
                               Buffer => Buffer);
         end loop;
         Fct.Sync_Obj.Finished;
      end loop;
   end Chunk_Task;

end Fractal;

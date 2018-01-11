with Ada.Exceptions;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
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
      procedure Free is new Ada.Unchecked_Deallocation (Row_Counter,
                                                        Row_Counter_Ptr);
   begin
      Self.Width := Width;
      Self.Height := Height;
      
      Self.Calculate_Imaginary_Bounds;
      Self.Calculate_Step;
      
      if Self.Cntr_Object /= null then
         Free (Self.Cntr_Object);
      end if;
      
      Self.Cntr_Object := new Row_Counter (Max => Self.Height);
   end Set_Size;
   
   function Get_Coordinate (Self : Abstract_Fractal;
                            X    : Natural;
                            Y    : Natural)
                            return Complex
   is
      Real_Coord : Float := Real_Min + (Float (X) * Self.Real_Step);
      Imag_Coord : Float := Self.Imag_Min + (Float (Y) * Self.Imag_Step);
   begin
      return Compose_From_Cartesian (Re => Real_Coord,
                                     Im => Imag_Coord);
   end Get_Coordinate;
                                                  
   procedure Calculate_Imaginary_Bounds (Self : in out Abstract_Fractal)
   is
      Inv_Aspect_Ratio : constant Float := Float (Self.Height) / 
                           Float (Self.Width);
   begin
      Self.Imag_Max := Inv_Aspect_Ratio * (Real_Max - Real_Min) / 2.0;
      Self.Imag_Min := (-1.0) * Self.Imag_Max;
   end Calculate_Imaginary_Bounds;
   
   procedure Calculate_Step (Self : in out Abstract_Fractal)
   is
   begin
      Self.Real_Step := (Real_Max - Real_Min) / Float (Self.Width);
      Self.Imag_Step := (Self.Imag_Max - Self.Imag_Min) / Float (Self.Height);
   end Calculate_Step;
   
   procedure Calculate_Image (Self : Abstract_Fractal;
                              Esc  : Float;
                              Buffer : out Stream_Element_Array_Access)
   is
   begin     
      
      for I in Self.Task_Pool'Range loop
         Self.Task_Pool (I).Go (Cntr  => Self.Cntr_Object,
                                E     => Esc,
                                Buf   => Buffer);
      end loop;
      
      Self.Cntr_Object.Wait_For_Complete;
      
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
         GNAT.OS_Lib.OS_Exit (-1);
   end Calculate_Image;
   
   procedure Calculate_Pixel_Color (Self  : Abstract_Fractal;
                                    Z_Mod : Float;
                                    Iters  : Natural;
                                    Px    : out Pixel)
   is
      Value : Integer := 765 * (Iters - 1) / Max_Iterations;
   begin
      if Z_Mod > 2.0 then
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
                            Esc  : Float;
                            Y    : ImgHeight;
                            Idx  : Stream_Element_Offset;
                            Buffer : out Stream_Element_Array_Access)
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
   
   protected body Row_Counter is
      
      procedure Get_Row (Row  : out Integer)
      is
      begin         
         if Cur_Row > Max then
            Row := -1;
         else
            Row := Cur_Row;
            Cur_Row := Cur_Row + 1;
         end if;
      end Get_Row;
      
      procedure Finished
      is
      begin
         Complete_Tasks := Complete_Tasks + 1;
      end Finished;
      
      entry Wait_For_Complete when Complete_Tasks = Task_Pool_Size is
      begin
         Complete_Tasks := 0;
         Cur_Row := ImgHeight'First;
      end Wait_For_Complete;
      
   end Row_Counter;
   
   task body Row_Task
   is
      Row     : Integer;
      Counter : Row_Counter_Ptr;
      Fct     : Abstract_Fractal_Ptr;
      Esc     : Float;
      Buffer  : Stream_Element_Array_Access;
   begin
      accept Initialize (F : Abstract_Fractal_Ptr) do
         Fct := F;
      end Initialize;
      
      loop
         accept Go (Cntr  : Row_Counter_Ptr;
                    E     : Float;
                    Buf   : Stream_Element_Array_Access) do
            Counter := Cntr;
            Esc := E;
            Buffer := Buf;
         end Go;

         loop
            Counter.Get_Row (Row);
            exit when Row = -1;
            
            Fct.Calculate_Row (Esc  => Esc,
                               Y    => Row,
                               Idx  => Buffer'First + 
                                 Stream_Element_Offset ((Row - 1) * Fct.Get_Width * Pixel'Size / 8),
                               Buffer => Buffer);
         end loop;
         Fct.Cntr_Object.Finished;
      end loop;
   end Row_Task;  
    
end Fractal;

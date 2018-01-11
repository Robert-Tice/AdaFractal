with Ada.Numerics.Complex_Types; use Ada.Numerics.Complex_Types;
with Ada.Streams; use Ada.Streams;

with AWS.Utils; use AWS.Utils;

with Image_Types; use Image_Types;

package Fractal is

   type Abstract_Fractal is abstract tagged limited private;
   type Abstract_Fractal_Ptr is access all Abstract_Fractal'Class;
   
   procedure Initialize (Frct_Ptr : Abstract_Fractal_Ptr);
   
   procedure Set_Size (Self   : in out Abstract_Fractal;
                       Width  : ImgWidth;
                       Height : ImgHeight);
   
   function Get_Width (Self : Abstract_Fractal) return ImgWidth;
   function Get_Height (Self : Abstract_Fractal) return ImgHeight;
   
   function Get_Buffer_Size (Self : Abstract_Fractal) return Stream_Element_Offset;
   
   function Get_Coordinate (Self : Abstract_Fractal;
                            X    : Natural;
                            Y    : Natural) 
                            return Complex;
   
   procedure Calculate_Image (Self   : Abstract_Fractal;
                              C      : Complex;
                              Buffer : out Stream_Element_Array_Access);
   
   procedure Calculate_Pixel (Self : Abstract_Fractal; 
                              C    : Complex;
                              X    : ImgWidth;
                              Y    : ImgHeight;
                              Px   : out Pixel)
   is abstract;
   
private
   
   Real_Max : constant := 2.0;
   Real_Min : constant := (-1) * Real_Max;
   
   protected type Row_Counter (Max : Integer) is
      procedure Get_Row (Row  : out Integer);
      procedure Finished;
      
      entry Wait_For_Complete;
   private
      Cur_Row : ImgHeight := ImgHeight'First;
      
      Complete_Tasks : Natural := 0;
   end Row_Counter;
   
   type Row_Counter_Ptr is access all Row_Counter;
   
   task type Row_Task is
      entry Initialize (F : Abstract_Fractal_Ptr);
      entry Go (Cntr  : Row_Counter_Ptr;
                Cmplx : Complex; 
                Buf   : Stream_Element_Array_Access);
   end Row_Task;
   
   type Row_Task_Pool is array (Natural range <>) of Row_Task;
   
   Task_Pool_Size : constant := 4;
   
   type Abstract_Fractal is abstract tagged limited record
      Width  : ImgWidth := ImgWidth'Last;
      Height : ImgHeight := ImgHeight'Last;
     
      Real_Step : Float;
      
      Imag_Max : Float;
      Imag_Min : Float;
      Imag_Step : Float;
      
      Task_Pool : Row_Task_Pool (1 .. Task_Pool_Size);
      
      Cntr_Object : Row_Counter_Ptr;
   end record;
   
   procedure Calculate_Row (Self : Abstract_Fractal;
                            C    : Complex;
                            Y    : ImgHeight;
                            Idx  : Stream_Element_Offset;
                            Buffer : out Stream_Element_Array_Access);
   
   procedure Calculate_Imaginary_Bounds (Self : in out Abstract_Fractal);
   
   procedure Calculate_Step (Self : in out Abstract_Fractal);
   
   function Get_Width (Self : Abstract_Fractal) return ImgWidth is
     (Self.Width);
   
   function Get_Height (Self : Abstract_Fractal) return ImgHeight is
     (Self.Height);
   
   function Get_Buffer_Size (Self : Abstract_Fractal) return Stream_Element_Offset is
     (Stream_Element_Offset (Self.Width * Self.Height * (Pixel'Size / 8)));
                                         
end Fractal;

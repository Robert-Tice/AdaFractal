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

   function Get_Buffer_Size (Self : Abstract_Fractal)
                             return Stream_Element_Offset;

--   type Complex_Type is new Float;

   D_Small : constant := 1.0 / (2.0 ** 13);
   type Complex_Type is delta D_Small range -100_000.0 .. 100_000.0 - D_Small;

   type Complex_Coordinate is record
      Re : Complex_Type;
      Im : Complex_Type;
   end record;

   function Get_Coordinate (Self : Abstract_Fractal;
                            X    : Natural;
                            Y    : Natural)
                            return Complex_Coordinate;

   procedure Calculate_Image (Self   : in out Abstract_Fractal;
                              Esc    : Complex_Type;
                              Buffer : out Stream_Element_Array_Access);

   Max_Iterations : constant := Color'Last / 5;

   procedure Calculate_Pixel (Self : Abstract_Fractal;
                              Esc  : Complex_Type;
                              X    : ImgWidth;
                              Y    : ImgHeight;
                              Px   : out Pixel)
   is abstract;

   procedure Calculate_Pixel_Color (Self  : Abstract_Fractal;
                                    Z_Mod : Complex_Type;
                                    Iters  : Natural;
                                    Px     : out Pixel);

   function Get_Frame (Self : in out Abstract_Fractal) return Color;

private

   Real_Max : constant := 2.0;
   Real_Min : constant := (-1) * Real_Max;

   protected type Pool_Sync
   is
      procedure Finished;
      entry Wait_For_Complete;
   private
      Complete_Tasks : Natural := 0;
   end Pool_Sync;

   task type Chunk_Task is
      pragma Priority (0);
      entry Initialize (F : Abstract_Fractal_Ptr);
      entry Go (Start_Row  : Natural;
                Stop_Row   : Natural;
                E      : Complex_Type;
                Buf    : Stream_Element_Array_Access);
   end Chunk_Task;

   type Chunk_Task_Pool is array (Natural range <>) of Chunk_Task;

   Task_Pool_Size : constant := 8;

   type Abstract_Fractal is abstract tagged limited record
      Width  : ImgWidth := ImgWidth'Last;
      Height : ImgHeight := ImgHeight'Last;

      Real_Step : Complex_Type;

      Imag_Max : Complex_Type;
      Imag_Min : Complex_Type;
      Imag_Step : Complex_Type;

      Task_Pool : Chunk_Task_Pool (1 .. Task_Pool_Size);

      Sync_Obj : Pool_Sync;

      Frame_Counter : Color := Color'First;
      Cnt_Up : Boolean := True;
   end record;

   procedure Calculate_Row (Self : Abstract_Fractal;
                            Esc  : Complex_Type;
                            Y    : ImgHeight;
                            Idx  : Stream_Element_Offset;
                            Buffer : out not null Stream_Element_Array_Access);

   procedure Calculate_Imaginary_Bounds (Self : in out Abstract_Fractal);

   procedure Calculate_Step (Self : in out Abstract_Fractal);

   function Get_Width (Self : Abstract_Fractal) return ImgWidth is
     (Self.Width);

   function Get_Height (Self : Abstract_Fractal) return ImgHeight is
     (Self.Height);

   function Get_Buffer_Size (Self : Abstract_Fractal)
                             return Stream_Element_Offset is
     (Stream_Element_Offset (Self.Width * Self.Height * (Pixel'Size / 8)));

end Fractal;

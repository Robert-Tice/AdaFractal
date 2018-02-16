with Ada.Streams; use Ada.Streams;

with AWS.Utils; use AWS.Utils;

with Computation_Type;
with Image_Types; use Image_Types;

generic
   with package CT is new Computation_Type (<>);
   with procedure Calculate_Pixel (Esc         : CT.Real;
                                   Re          : CT.Real;
                                   Im          : CT.Real;
                                   Z_Escape    : out CT.Real;
                                   Iter_Escape : out Natural);
package Fractal is
   use CT;

   procedure Init (Viewport : Viewport_Info);

   procedure Set_Size (Viewport : Viewport_Info);

   procedure Calculate_Image (Esc    : Real;
                              Buffer : out Stream_Element_Array_Access);

   function Get_Frame return Color;

   function Get_Buffer_Size return Stream_Element_Offset;

private

   Real_Distance_Unzoomed : constant Real := To_Real (4);

   protected type Pool_Sync
   is
      procedure Finished;
      entry Wait_For_Complete;
   private
      Complete_Tasks : Natural := 0;
   end Pool_Sync;

   task type Chunk_Task is
      pragma Priority (0);
      entry Go (Start_Row  : Natural;
                Stop_Row   : Natural;
                E          : Real;
                Buf        : Stream_Element_Array_Access);
   end Chunk_Task;

   type Chunk_Task_Pool is array (Natural range <>) of Chunk_Task;

   type Complex_Coordinate is record
      Re : Real;
      Im : Real;
   end record;

   Task_Pool_Size : constant := 8;

   S_Width        : ImgWidth;
   S_Height       : ImgHeight;
   S_Zoom         : ImgZoom;
   S_Center       : Complex_Coordinate;

   S_Real_Step : Real;

   S_Imag_Max : Real;
   S_Imag_Min : Real;

   S_Real_Max : Real;
   S_Real_Min : Real;

   S_Imag_Step : Real;

   S_Task_Pool : Chunk_Task_Pool (1 .. Task_Pool_Size);

   S_Sync_Obj : Pool_Sync;

   S_Frame_Counter : Color := Color'First;
   S_Cnt_Up        : Boolean := True;


   procedure Calculate_Row (Esc  : Real;
                            Y    : ImgHeight;
                            Idx  : Stream_Element_Offset;
                            Buffer : out not null Stream_Element_Array_Access);

   procedure Calculate_Bounds;

   procedure Calculate_Step;

   procedure Calculate_Pixel_Color (Z_Escape     : Real;
                                    Iter_Escape  : Natural;
                                    Px           : out Pixel);

   function Get_Coordinate (X : ImgWidth;
                            Y : ImgHeight)
                            return Complex_Coordinate;

   function Get_Coordinate (Coord : Coordinate) return Complex_Coordinate is
     (Get_Coordinate (X => Coord.X,
                      Y => Coord.Y));

   function Get_Width return ImgWidth is
     (S_Width);

   function Get_Height return ImgHeight is
     (S_Height);

   function Get_Buffer_Size return Stream_Element_Offset is
     (Stream_Element_Offset (S_Width * S_Height * (Pixel'Size / 8)));

end Fractal;

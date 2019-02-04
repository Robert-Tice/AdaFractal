with Ada.Streams; use Ada.Streams;
with Ada.Synchronous_Barriers; use Ada.Synchronous_Barriers;

with AWS.Utils; use AWS.Utils;

with Computation_Type;
with Image_Types; use Image_Types;

generic
   with package CT is new Computation_Type (<>);
   with procedure Calculate_Pixel (Re          : CT.Real;
                                   Im          : CT.Real;
                                   Z_Escape    : out CT.Real;
                                   Iter_Escape : out Natural);
   Task_Pool_Size : Natural;
package Fractal is
   use CT;

   procedure Init (Viewport : Viewport_Info);

   procedure Set_Size (Viewport : Viewport_Info);

   procedure Calculate_Image (Buffer : not null Stream_Element_Array_Access);

   function Get_Frame return Color;

   function Get_Buffer_Size return Stream_Element_Offset;

private

   Real_Distance_Unzoomed : constant Real := To_Real (4);

   task type Chunk_Task_Type is
      pragma Priority (0);
      entry Go (Start_Row : Natural;
                Stop_Row  : Natural;
                Buf       : Stream_Element_Array_Access);
   end Chunk_Task_Type;

   type Chunk_Task is record
      T : Chunk_Task_Type;
      Start_Row : Natural;
      Stop_Row  : Natural;
   end record;

   type Chunk_Task_Pool is array (1 .. Task_Pool_Size) of Chunk_Task;

   type Complex_Coordinate is record
      Re : Real;
      Im : Real;
   end record;

   S_Width        : ImgWidth;
   S_Height       : ImgHeight;
   S_Zoom         : ImgZoom;
   S_Center       : Complex_Coordinate;

   S_Step         : Complex_Coordinate;
   S_Max          : Complex_Coordinate;
   S_Min          : Complex_Coordinate;

   S_Task_Pool : Chunk_Task_Pool;
   S_Sync_Obj  : Synchronous_Barrier (Release_Threshold => Task_Pool_Size + 1);

   S_Frame_Counter : Color := Color'First;
   S_Cnt_Up        : Boolean := True;


   procedure Calculate_Row (Y      : ImgHeight;
                            Idx    : Stream_Element_Offset;
                            Buffer : not null Stream_Element_Array_Access);

   procedure Calculate_Bounds;

   procedure Calculate_Step;

   procedure Update_Task_Rows;

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

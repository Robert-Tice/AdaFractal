with Ada.Streams; use Ada.Streams;

with AWS.Response;
with AWS.Status;

with Computation_Type;
with Image_Types;
with Fractal;
with Julia_Set;

package Router_Cb is
   function Router (Request : AWS.Status.Data) return AWS.Response.Data;

   procedure Init;

   Server_Alive  : Boolean := True;

   type Color is new Natural range 0 .. 255
     with Size => 8;

   Max_Iterations : constant := Color'Last / 5;

   type RGB888_Pixel is record
      Red   : Color;
      Green : Color;
      Blue  : Color;
      Alpha : Color;
   end record
     with Size => 32;

   for RGB888_Pixel use record
      Red at 0 range 0 .. 7;
      Green at 1 range 0 .. 7;
      Blue at 2 range 0 .. 7;
      Alpha at 3 range 0 .. 7;
   end record;

   procedure Color_Pixel (Z_Escape    : Boolean;
                          Iter_Escape : Natural;
                          Px          : out RGB888_Pixel);

   package RGB888_IT is new Image_Types (Pixel          => RGB888_Pixel,
                                         Color_Pixel    => Color_Pixel,
                                         Max_Iterations => Max_Iterations);
   use RGB888_IT;

   Max_Buffer_Size : constant Buffer_Offset :=
                       Buffer_Offset (ImgWidth'Last * ImgHeight'Last *  (RGB888_Pixel'Size / 8));

   RawData : Buffer_Access :=
               new Buffer_Array (1 .. Max_Buffer_Size);

private
   Task_Pool_Size : constant := 16;

   Compute_Time : Duration := Duration'First;

   Viewport : Viewport_Info;

   type Computation_Enum is
     (Fixed_Type, Float_Type);

   Frame_Counter  : Color := 0;
   Cnt_Up : Boolean := True;

   procedure Increment_Frame;

   type Real_Float is new Float;

   function Integer_To_Float (V : Integer) return Real_Float is
     (Real_Float (V));

   function Float_To_Integer (V : Real_Float) return Integer is
     (Natural (V));

   function Float_To_Real_Float (V : Float) return Real_Float is
     (Real_Float (V));

   function Real_Float_To_Float (V : Real_Float) return Float is
      (Float (V));

   function Float_Image (V : Real_Float) return String is
     (V'Img);

   D_Small : constant := 2.0 ** (-21);
   type Real_Fixed is delta D_Small range -100.0 .. 201.0 - D_Small;

   function "*" (Left, Right : Real_Fixed) return Real_Fixed;
   pragma Import (Intrinsic, "*");

   function "/" (Left, Right : Real_Fixed) return Real_Fixed;
   pragma Import (Intrinsic, "/");

   function Integer_To_Fixed (V : Integer) return Real_Fixed is
     (Real_Fixed (V));

   function Float_To_Fixed (V : Float) return Real_Fixed is
     (Real_Fixed (V));

   function Fixed_To_Float (V : Real_Fixed) return Float is
      (Float (V));

   function Fixed_To_Integer (V : Real_Fixed) return Integer is
     (Natural (V));

   function Fixed_Image (V : Real_Fixed) return String is
      (V'Img);

   package Fixed_Computation is new Computation_Type (Real       => Real_Fixed,
                                                      "*"        => Router_Cb."*",
                                                      "/"        => Router_Cb."/",
                                                      To_Real    => Integer_To_Fixed,
                                                      F_To_Real  => Float_To_Fixed,
                                                      To_Integer => Fixed_To_Integer,
                                                      To_Float   => Fixed_To_Float,
                                                      Image      => Fixed_Image);

   package Fixed_Julia is new Julia_Set (CT               => Fixed_Computation,
                                         IT               => RGB888_IT,
                                         Escape_Threshold => 100.0);

   package Fixed_Julia_Fractal is new Fractal (CT              => Fixed_Computation,
                                               IT              => RGB888_IT,
                                               Calculate_Pixel => Fixed_Julia.Calculate_Pixel,
                                               Task_Pool_Size  => Task_Pool_Size);


   package Float_Computation is new Computation_Type (Real       => Real_Float,
                                                      To_Real    => Integer_To_Float,
                                                      F_To_Real  => Float_To_Real_Float,
                                                      To_Integer => Float_To_Integer,
                                                      To_Float   => Real_Float_To_Float,
                                                      Image      => Float_Image);

   package Float_Julia is new Julia_Set (CT               => Float_Computation,
                                         IT               => RGB888_IT,
                                         Escape_Threshold => 100.0);

   package Float_Julia_Fractal is new Fractal (CT              => Float_Computation,
                                               IT              => RGB888_IT,
                                               Calculate_Pixel => Float_Julia.Calculate_Pixel,
                                               Task_Pool_Size  => Task_Pool_Size);

   procedure ImgSize_Parse (URI : String);

   function Compute_Image (Comp_Type : Computation_Enum)
                           return Buffer_Offset;

   procedure Reset_Viewport;

end Router_Cb;

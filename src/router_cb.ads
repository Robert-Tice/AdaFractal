with Ada.Streams; use Ada.Streams;

with AWS.Response;
with AWS.Status;
with AWS.Utils; use AWS.Utils;

with Computation_Type;
with Image_Types; use Image_Types;
with Fractal;
with Julia_Set;

package Router_Cb is
   function Router (Request : AWS.Status.Data) return AWS.Response.Data;

   procedure Init;

   Server_Alive  : Boolean := True;

   Max_Buffer_Size : constant :=
                       ImgWidth'Last * ImgHeight'Last * (Pixel_Size);

   RawData : Stream_Element_Array_Access :=
               new Stream_Element_Array (1 .. Max_Buffer_Size);

private

   Compute_Time : Duration := Duration'First;

   Viewport : Viewport_Info;

   type Computation_Enum is
     (Fixed_Type, Float_Type);

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
                                         Escape_Threshold => 100.0);

   package Fixed_Julia_Fractal is new Fractal (CT              => Fixed_Computation,
                                               Calculate_Pixel => Fixed_Julia.Calculate_Pixel);


   package Float_Computation is new Computation_Type (Real       => Real_Float,
                                                      To_Real    => Integer_To_Float,
                                                      F_To_Real  => Float_To_Real_Float,
                                                      To_Integer => Float_To_Integer,
                                                      To_Float   => Real_Float_To_Float,
                                                      Image      => Float_Image);

   package Float_Julia is new Julia_Set (CT               => Float_Computation,
                                         Escape_Threshold => 100.0);

   package Float_Julia_Fractal is new Fractal (CT              => Float_Computation,
                                               Calculate_Pixel => Float_Julia.Calculate_Pixel);

   procedure ImgSize_Parse (URI : String);

   function Compute_Image (Comp_Type : Computation_Enum)
                           return Stream_Element_Offset;

   procedure Reset_Viewport;

end Router_Cb;

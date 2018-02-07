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

   Server_Alive  : Boolean := True;

   Max_Buffer_Size : constant :=
                       ImgWidth'Last * ImgHeight'Last * (Pixel_Size);

   RawData : Stream_Element_Array_Access :=
               new Stream_Element_Array (1 .. Max_Buffer_Size);

private
   type Real_Float is new Float;

   function Natural_To_Float (V : Natural) return Real_Float is
     (Real_Float (V));

   function Float_To_Natural (V : Real_Float) return Natural is
      (Natural (V));

   D_Small : constant := 1.0 / (2.0 ** 13);
   type Real_Fixed is delta D_Small range -100_000.0 .. 100_000.0 - D_Small;

   function "*" (Left, Right : Real_Fixed) return Real_Fixed;
   pragma Import (Intrinsic, "*");

   function "/" (Left, Right : Real_Fixed) return Real_Fixed;
   pragma Import (Intrinsic, "/");

   function Natural_To_Fixed (V : Natural) return Real_Fixed is
     (Real_Fixed (V));

   function Fixed_To_Natural (V : Real_Fixed) return Natural is
     (Natural (V));


   package Fixed_Computation is new Computation_Type (Real       => Real_Fixed,
                                                      "*"        => Router_Cb."*",
                                                      "/"        => Router_Cb."/",
                                                      To_Real    => Natural_To_Fixed,
                                                      To_Natural => Fixed_To_Natural);
   package Fixed_Julia is new Julia_Set (CT               => Fixed_Computation,
                                         Escape_Threshold => 100.0);
   package Fixed_Julia_Fractal is new Fractal (CT              => Fixed_Computation,
                                               Calculate_Pixel => Fixed_Julia.Calculate_Pixel);

   package Float_Computation is new Computation_Type (Real       => Real_Float,
                                                      To_Real    => Natural_To_Float,
                                                      To_Natural => Float_To_Natural);
   package Float_Julia is new Julia_Set (CT               => Float_Computation,
                                         Escape_Threshold => 100.0);
   package Float_Julia_Fractal is new Fractal (CT              => Float_Computation,
                                               Calculate_Pixel => Float_Julia.Calculate_Pixel);

   procedure ImgSize_Parse (URI : String);




end Router_Cb;

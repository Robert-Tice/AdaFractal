with Fractal; use Fractal;
with Image_Types; use Image_Types;

package Julia_Set is

   type Julia_Fractal is new Abstract_Fractal with null record;

private

   procedure Calculate_Pixel (Self : Julia_Fractal;
                              Esc  : Complex_Type;
                              X    : ImgWidth;
                              Y    : ImgHeight;
                              Px   : out Pixel);

end Julia_Set;

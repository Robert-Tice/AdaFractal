with Computation_Type;
with Image_Types; use Image_Types;

generic
   with package CT is new Computation_Type (<>);
   Escape_Threshold : CT.Real;
package Julia_Set is
   use CT;

   procedure Calculate_Pixel (Re          : Real;
                              Im          : Real;
                              Z_Escape    : out Real;
                              Iter_Escape : out Natural);

end Julia_Set;

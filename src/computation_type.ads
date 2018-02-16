generic
   type Real is private;
   with function "*" (Left, Right : Real) return Real is <>;
   with function "/" (Left, Right : Real) return Real is <>;
   with function To_Real (V : Integer) return Real is <>;
   with function F_To_Real (V : Float) return Real is <>;
   with function To_Integer (V : Real) return Integer is <>;
   with function To_Float (V : Real) return Float is <>;
   with function Image (V : Real) return String is <>;
   with function "+" (Left, Right : Real) return Real is <>;
   with function "-" (Left, Right : Real) return Real is <>;
   with function ">" (Left, Right : Real) return Boolean is <>;
   with function "<" (Left, Right : Real) return Boolean is <>;
   with function "<=" (Left, Right : Real) return Boolean is <>;
   with function ">=" (Left, Right : Real) return Boolean is <>;
package Computation_Type is    

end Computation_Type;

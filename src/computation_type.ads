generic
   type Real is private;
   with function "*" (Left, Right : Real) return Real is <>;
   with function "/" (Left, Right : Real) return Real is <>;
   with function To_Real (V : Natural) return Real is <>;
   with function To_Natural (V : Real) return Natural is <>;
   with function "+" (Left, Right : Real) return Real is <>;
   with function "-" (Left, Right : Real) return Real is <>;
   with function ">" (Left, Right : Real) return Boolean is <>;
   with function "<" (Left, Right : Real) return Boolean is <>;
   with function "<=" (Left, Right : Real) return Boolean is <>;
   with function ">=" (Left, Right : Real) return Boolean is <>;
package Computation_Type is    

end Computation_Type;

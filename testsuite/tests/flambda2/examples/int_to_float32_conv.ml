(* TEST
 arch64;
 include stdlib_stable;
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Constant-folding an integer -> float32 conversion must produce the same
   result as the backend (a single [cvtsi2ss] rounding), rather than
   double-rounding through a 64-bit float. The witnesses below have distinct
   single- and double-rounded values:

   - 9007199791611905 = 2^53 + 2^29 + 1:
       single-rounded (correct)  = 0x5a000001 = 0x1.000002p+53s
       double-rounded  (bug)     = 0x5a000000 = 0x1p+53s
   - 36028799166447617 = 2^55 + 2^31 + 1:
       single-rounded (correct)  = 0x5b000001 = 0x1.000002p+55s
       double-rounded  (bug)     = 0x5b000000 = 0x1p+55s *)

let a () = Stdlib_stable.Float32.of_int 9007199791611905

let b () = Stdlib_stable.Float32.of_int 36028799166447617

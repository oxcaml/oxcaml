(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* A float32 literal must be parsed to the same value the backend produces (a
   single, correctly-rounded [strtof] of the decimal), rather than being parsed
   to a 64-bit float and then rounded to float32 (a double rounding). The
   witnesses below have distinct single- and double-rounded values:

   - 9007199791611905 = 2^53 + 2^29 + 1:
       single-rounded (correct)  = 0x5a000001 = 0x1.000002p+53s
       double-rounded  (bug)     = 0x5a000000 = 0x1p+53s
   - 36028799166447617 = 2^55 + 2^31 + 1:
       single-rounded (correct)  = 0x5b000001 = 0x1.000002p+55s
       double-rounded  (bug)     = 0x5b000000 = 0x1p+55s *)

let a () = 9007199791611905.s

let b () = 36028799166447617.s

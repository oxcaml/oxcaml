(* TEST
 modules = "args_stubs.c";
 reference = "${test_source_directory}/unboxed_args_reaper.reference";
 include stdlib_stable;
 flambda2;
 {
   flags = "-extension layouts_beta -flambda2-reaper -reaper-local-fields";
   native;
 }
*)

(* The nested-product cases of unboxed_args.ml, compiled with the reaper
   enabled. The reaper rewrites the arities of applications and must preserve
   their structure, since that determines the C struct layout of unboxed
   product arguments and returns (see [To_cmm_extcall]). *)

open Stdlib_stable

external untag_int16 : int16 -> int16# = "%untag_int16"
external tag_int16 : int16# -> int16 = "%tag_int16"
external box_int64 : int64_u -> (int64[@local_opt]) = "%box_int64"
external box_int32 : int32_u -> (int32[@local_opt]) = "%box_int32"
external unbox_int32 : (int32[@local_opt]) -> int32_u = "%unbox_int32"

let i32 x = unbox_int32 x
let i16 x = untag_int16 (Int16.of_int x)
let i16' x = Int16.to_int (tag_int16 x)

external sum_nested_padded : #(#(int32_u * int16#) * int16#) -> int64_u
  = "sum_nested_padded_bytecode" "sum_nested_padded_native"

let () =
  Printf.eprintf "sum_nested_padded: %Ld\n%!"
    (box_int64 (sum_nested_padded #(#(i32 100000l, i16 (-300)), i16 7)))

external ret_nested_padded : unit -> #(#(int32_u * int16#) * int16#)
  = "ret_nested_padded_bytecode" "ret_nested_padded_native"

let () =
  let #(#(a, b), c) = ret_nested_padded () in
  Printf.eprintf "ret_nested_padded: %ld %d %d\n%!" (box_int32 a) (i16' b)
    (i16' c)

(* TEST
 modules = "fat_ptr_simd_c.c";
 reference = "${test_source_directory}/fat_ptr_simd.reference";
 arch_amd64;
 flambda2;
 {
   flags = "-extension simd_beta";
   native;
 }
*)

(* Fat pointers mixed with a stack-passed SIMD vector: the two fat-pointer
   offsets do not fit in integer argument registers (six are taken) and are
   passed in extra stack words following the 32-byte [int64x4]; the outgoing
   stack area must then be padded back to the vector's 32-byte alignment. *)

external const1 : int64 -> int64x4 = "" "caml_int64x4_const1"
  [@@noalloc] [@@unboxed] [@@builtin]

external mix_simd
  :  (#(bytes * int64_u)[@fat_ptr])
  -> (#(bytes * int64_u)[@fat_ptr])
  -> int64_u
  -> int64_u
  -> int64_u
  -> int64_u
  -> float#
  -> float#
  -> float#
  -> float#
  -> float#
  -> float#
  -> float#
  -> float#
  -> (int64x4[@unboxed])
  -> (int[@untagged])
  = "fat_ptr_simd_bytecode_unused" "test_fat_simd_native"
  [@@noalloc]

let () =
  let p = Bytes.of_string "\001\002\003\004" in
  let q = Bytes.of_string "\005\006\007\008" in
  let v = const1 100L in
  (* p.[1] + q.[2] + (10+20+30+40) + 8 * 0.5 + 4 * 100
     = 2 + 7 + 100 + 4 + 400 = 513 *)
  Printf.printf "fat-simd: %d\n"
    (mix_simd #(p, #1L) #(q, #2L) #10L #20L #30L #40L #0.5 #0.5 #0.5 #0.5
       #0.5 #0.5 #0.5 #0.5 v)

(* TEST
 flambda2;
 {
   flags = "-extension layouts_beta";
   { expect; expect.opt; }
 }{
   flags = "-extension layouts_beta -Oclassic";
   expect.opt;
 }{
   flags = "-extension layouts_beta -O3";
   expect.opt;
 }
*)

(* All-void records created by instantiating [any] with a [void] type
   must act as if they were defined directly, without [any].
   This file tests behaviour that must agree across backends;
   representation is observed in [all_void_records_native.ml]. *)

type ('a : any) r = { field : 'a }
[%%expect{|
type ('a : any) r = { field : 'a; }
|}]

let proj_result =
  let r : unit# r = { field = #() } in
  let #() = r.field in
  "projected"
[%%expect{|
val proj_result : string = "projected"
|}]

let match_result =
  match ({ field = #(#(), #()) } : #(unit# * unit#) r) with
  | { field = #(v, _) } -> let #() = v in "matched"
[%%expect{|
val match_result : string = "matched"
|}]

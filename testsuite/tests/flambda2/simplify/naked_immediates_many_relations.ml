(* TEST
   compile_only = "true";
   flambda2;
   expect.opt with dump-simplify;
 *)

[@@@ocaml.flambda_o3]


type t = A of (int -> int) | B of (int -> int)

(* It used to be that we were only able to record a single relation between a
   naked immediate value and a variant. This test shows a situation where it is
   useful to be able to have multiple such relations.

   When tracking a single relation, in the re-simplification of [k] when it is
   called as the result of [r], we forget its relation with either [x] or [y],
   and can only inline one of the two calls in each branch -- the other remain
   as an indirect call.
 *)

let r =
  let f x = x in
  let g x = x in
  let k x y =
    match x, y with
    | A _, A _ | B _, B _ ->
      begin match x, y with
      | A ax, A ay -> ax (ay 0)
      | B bx, B by -> by (bx 0)
      | _ -> assert false
      end
    | _ -> assert false
  in
  let x = if Sys.opaque_identity true then A f else B g in
  let y = if Sys.opaque_identity true then A g else B f in
  (k[@inlined]) x y
[%%expect_fexpr Simplify{|
let $camlTOP3__immstring54 = "" in
let $camlTOP3__const_block56 = Block 0 ($camlTOP3__immstring54, 27, 11) in
let $camlTOP3__Pmakeblock59 =
  Block 0 ($`*predef*`.caml_exn_Assert_failure, $camlTOP3__const_block56)
in
(let Popaque = %opaque (1) in
 let untagged = %untag_imm (Popaque) in
 switch untagged
   | 0 -> k1 (1i)
   | 1 -> k1 (0i))
  where k1 (tag : imm) =
    ((let Popaque = %opaque (1) in
      let untagged = %untag_imm (Popaque) in
      switch untagged
        | 0 -> k1 (1i)
        | 1 -> k1 (0i))
       where k1 (tag_1 : imm) =
         (switch tag
            | 0 -> k1
            | 1 -> k2
            where k2 =
              switch tag_1
                | 0 -> error pop(regular error) ($camlTOP3__Pmakeblock59)
                | 1 -> k
            where k1 =
              switch tag_1
                | 0 -> k
                | 1 -> error pop(regular error) ($camlTOP3__Pmakeblock59)))
  where k =
    let $camlTOP3 = Block 0 (0) in
    cont done ($camlTOP3)
|}]

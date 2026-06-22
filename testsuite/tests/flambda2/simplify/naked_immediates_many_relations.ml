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

let r b =
  let f x = x in
  let g x = x in
  let k x y =
    match x, y with
    | A _, A _ | B _, B _ ->
      begin match x, y with
      | A ax, A ay -> ax (ay 0)
      | B bx, B by -> by (bx 0)
      end
  in
  let x = if b then A f else B g in
  let y = if b then A g else B f in
  (k[@inlined]) x y
[%%expect_fexpr Simplify{|
let $camlTOP3__immstring68 = "" in
let $camlTOP3__const_block76 = Block 0 ($camlTOP3__immstring68, 19, 4) in
let $camlTOP3__Pmakeblock79 =
  Block 0 ($`*predef*`.caml_exn_Match_failure, $camlTOP3__const_block76)
in
let code r_0 deleted in
let code loopify(never) size(52) newer_version_of(r_0)
      r_0_1 (b : imm tagged)
        my_closure _region _ghost_region my_depth
        -> k * k1
        : imm tagged =
  (let untagged = %untag_imm (b) in
   switch untagged
     | 0 -> k2 (1i)
     | 1 -> k2 (0i))
    where k2 (tag : imm) =
      ((let untagged = %untag_imm (b) in
        switch untagged
          | 0 -> k2 (1i)
          | 1 -> k2 (0i))
         where k2 (tag_1 : imm) =
           (switch tag
              | 0 -> k2
              | 1 -> k3
              where k3 =
                switch tag_1
                  | 0 -> k1 pop(regular k1) ($camlTOP3__Pmakeblock79)
                  | 1 -> k (0)
              where k2 =
                switch tag_1
                  | 0 -> k (0)
                  | 1 -> k1 pop(regular k1) ($camlTOP3__Pmakeblock79)))
in
let $camlTOP3__r_3 = closure r_0_1 @r in
let $camlTOP3 = Block 0 ($camlTOP3__r_3) in
cont done ($camlTOP3)
|}]

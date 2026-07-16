[@@@ocaml.warnerror "+a-40-41-42"]

(* Regression test for the vectorization of integer comparisons
   ([Intop (Icomp _)]): the scalar instruction produces 0 or 1, whereas the
   vector comparison instructions (the pcmpeq/pcmpgt families) produce a 0 or all-ones
   mask per lane, so these groups must not be vectorized. The unboxed [int64#]
   fields let the untagged comparison result flow directly into the stores
   (the tagging of a boxed [int] result is an [Ilea Iscaled] which is not
   vectorizable and would hide the comparison). Before the fix, the fields
   compared equal (resp. greater) were set to -1 instead of 1. *)

module Int64_u = struct
  external to_int64 : int64# -> (int64[@local_opt]) = "%box_int64"
  [@@warning "-187"]

  external of_int64 : (int64[@local_opt]) -> int64# = "%unbox_int64"
  [@@warning "-187"]
end

type t2 =
  { mutable d0 : int64#;
    mutable d1 : int64#
  }

let[@opaque] eq_pair (a : t2) (b : t2) (dst : t2) =
  dst.d0
    <- Int64_u.of_int64
         (Int64.of_int
            (Bool.to_int
               (Int64.equal (Int64_u.to_int64 a.d0) (Int64_u.to_int64 b.d0))));
  dst.d1
    <- Int64_u.of_int64
         (Int64.of_int
            (Bool.to_int
               (Int64.equal (Int64_u.to_int64 a.d1) (Int64_u.to_int64 b.d1))))

let[@opaque] gt_pair (a : t2) (b : t2) (dst : t2) =
  dst.d0
    <- Int64_u.of_int64
         (Int64.of_int
            (Bool.to_int
               (Int64.compare (Int64_u.to_int64 a.d0) (Int64_u.to_int64 b.d0)
               > 0)));
  dst.d1
    <- Int64_u.of_int64
         (Int64.of_int
            (Bool.to_int
               (Int64.compare (Int64_u.to_int64 a.d1) (Int64_u.to_int64 b.d1)
               > 0)))

let print_t2 ppf (t : t2) =
  Format.fprintf ppf "{ d0 = %Ld ; d1 = %Ld }" (Int64_u.to_int64 t.d0)
    (Int64_u.to_int64 t.d1)

let () =
  let dst = { d0 = -#1L; d1 = -#1L } in
  eq_pair { d0 = #10L; d1 = #20L } { d0 = #10L; d1 = #30L } dst;
  Format.printf "eq_pair %a\n" print_t2 dst;
  let dst = { d0 = -#1L; d1 = -#1L } in
  gt_pair { d0 = #5L; d1 = #40L } { d0 = #3L; d1 = #40L } dst;
  Format.printf "gt_pair %a\n" print_t2 dst

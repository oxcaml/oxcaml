module I64 = Stdlib_upstream_compatible.Int64_u

let eq x y =
  if I64.equal x y then ()
  else Printf.printf "%Lx <> %Lx\n" (I64.to_int64 x) (I64.to_int64 y)

let maxi () = #0x7fff_ffff_ffff_ffffL
let mini () = #0x8000_0000_0000_0000L

(* (low,high) + (low,high) -> (low,high) *)
external add_int128 : x0:int64_u -> x1:int64_u -> y0:int64_u -> y1:int64_u -> #(int64_u * int64_u) = "" "caml_int128_add"
[@@noalloc] [@@builtin]

let () =
  let #(z0,z1) = add_int128 ~x0:#1L ~x1:#2L ~y0:#2L ~y1:#1L in
  eq z0 #3L;
  eq z1 #3L

let () =
  let #(z0,z1) = add_int128 ~x0:(maxi ()) ~x1:(mini ()) ~y0:#1L ~y1:(-#1L) in
  eq z0 (mini ());
  eq z1 (maxi ())

let () = (* (1<<64-1) + 1 *)
  let #(z0,z1) = add_int128 ~x0:(-#1L) ~x1:#0L ~y0:#1L ~y1:#0L in
  eq z0 #0L;
  eq z1 #1L

let () = (* 1<<64 + (-1) *)
  let #(z0,z1) = add_int128 ~x0:(#0L) ~x1:(#1L) ~y0:(-#1L) ~y1:(-#1L) in
  eq z0 (-#1L);
  eq z1 #0L

let () = (* (1<<127-1) + 1 *)
  let #(z0,z1) = add_int128 ~x0:(-#1L) ~x1:(maxi ()) ~y0:#1L ~y1:#0L in
  eq z0 #0L;
  eq z1 (mini ())

let () = (* -(1<<127) - 1 *)
  let #(z0,z1) = add_int128 ~x0:#0L ~x1:(mini ()) ~y0:(-#1L) ~y1:(-#1L) in
  eq z0 (-#1L);
  eq z1 (maxi ())

(* (low,high) - (low,high) -> (low,high) *)
external sub_int128 : x0:int64_u -> x1:int64_u -> y0:int64_u -> y1:int64_u -> #(int64_u * int64_u) = "" "caml_int128_sub"
[@@noalloc] [@@builtin]

let () =
  let #(z0,z1) = sub_int128 ~x0:#2L ~x1:#1L ~y0:#1L ~y1:#2L in
  eq z0 #1L;
  eq z1 (-#1L)

let () =
  let #(z0,z1) = sub_int128 ~x0:(maxi ()) ~x1:(mini ()) ~y0:(#1L) ~y1:(-#1L) in
  eq z0 I64.(sub (maxi ()) #1L);
  eq z1 I64.(add (mini ()) #1L)

let () = (* (1<<64-1) - (-1) *)
  let #(z0,z1) = sub_int128 ~x0:(-#1L) ~x1:#0L ~y0:(-#1L) ~y1:(-#1L) in
  eq z0 #0L;
  eq z1 #1L

let () = (* 1<<64 - 1 *)
  let #(z0,z1) = sub_int128 ~x0:(#0L) ~x1:(#1L) ~y0:(#1L) ~y1:#0L in
  eq z0 (-#1L);
  eq z1 #0L

let () = (* (1<<127-1) - (-1) *)
  let #(z0,z1) = sub_int128 ~x0:(-#1L) ~x1:(maxi ()) ~y0:(-#1L) ~y1:(-#1L) in
  eq z0 #0L;
  eq z1 (mini ())

let () = (* -(1<<127) - 1 *)
  let #(z0,z1) = sub_int128 ~x0:#0L ~x1:(mini ()) ~y0:(#1L) ~y1:#0L in
  eq z0 (-#1L);
  eq z1 (maxi ())

(* Every way of sharing operands between [x0], [x1], [y0] and [y1]: the name of
   each function gives the variable passed for each of them, in that order. *)

let[@inline never] add_aaaa a _ _ _ =
  add_int128 ~x0:a ~x1:a ~y0:a ~y1:a
let[@inline never] add_aaab a b _ _ =
  add_int128 ~x0:a ~x1:a ~y0:a ~y1:b
let[@inline never] add_aaba a b _ _ =
  add_int128 ~x0:a ~x1:a ~y0:b ~y1:a
let[@inline never] add_aabb a b _ _ =
  add_int128 ~x0:a ~x1:a ~y0:b ~y1:b
let[@inline never] add_aabc a b c _ =
  add_int128 ~x0:a ~x1:a ~y0:b ~y1:c
let[@inline never] add_abaa a b _ _ =
  add_int128 ~x0:a ~x1:b ~y0:a ~y1:a
let[@inline never] add_abab a b _ _ =
  add_int128 ~x0:a ~x1:b ~y0:a ~y1:b
let[@inline never] add_abac a b c _ =
  add_int128 ~x0:a ~x1:b ~y0:a ~y1:c
let[@inline never] add_abba a b _ _ =
  add_int128 ~x0:a ~x1:b ~y0:b ~y1:a
let[@inline never] add_abbb a b _ _ =
  add_int128 ~x0:a ~x1:b ~y0:b ~y1:b
let[@inline never] add_abbc a b c _ =
  add_int128 ~x0:a ~x1:b ~y0:b ~y1:c
let[@inline never] add_abca a b c _ =
  add_int128 ~x0:a ~x1:b ~y0:c ~y1:a
let[@inline never] add_abcb a b c _ =
  add_int128 ~x0:a ~x1:b ~y0:c ~y1:b
let[@inline never] add_abcc a b c _ =
  add_int128 ~x0:a ~x1:b ~y0:c ~y1:c
let[@inline never] add_abcd a b c d =
  add_int128 ~x0:a ~x1:b ~y0:c ~y1:d

let[@inline never] sub_aaaa a _ _ _ =
  sub_int128 ~x0:a ~x1:a ~y0:a ~y1:a
let[@inline never] sub_aaab a b _ _ =
  sub_int128 ~x0:a ~x1:a ~y0:a ~y1:b
let[@inline never] sub_aaba a b _ _ =
  sub_int128 ~x0:a ~x1:a ~y0:b ~y1:a
let[@inline never] sub_aabb a b _ _ =
  sub_int128 ~x0:a ~x1:a ~y0:b ~y1:b
let[@inline never] sub_aabc a b c _ =
  sub_int128 ~x0:a ~x1:a ~y0:b ~y1:c
let[@inline never] sub_abaa a b _ _ =
  sub_int128 ~x0:a ~x1:b ~y0:a ~y1:a
let[@inline never] sub_abab a b _ _ =
  sub_int128 ~x0:a ~x1:b ~y0:a ~y1:b
let[@inline never] sub_abac a b c _ =
  sub_int128 ~x0:a ~x1:b ~y0:a ~y1:c
let[@inline never] sub_abba a b _ _ =
  sub_int128 ~x0:a ~x1:b ~y0:b ~y1:a
let[@inline never] sub_abbb a b _ _ =
  sub_int128 ~x0:a ~x1:b ~y0:b ~y1:b
let[@inline never] sub_abbc a b c _ =
  sub_int128 ~x0:a ~x1:b ~y0:b ~y1:c
let[@inline never] sub_abca a b c _ =
  sub_int128 ~x0:a ~x1:b ~y0:c ~y1:a
let[@inline never] sub_abcb a b c _ =
  sub_int128 ~x0:a ~x1:b ~y0:c ~y1:b
let[@inline never] sub_abcc a b c _ =
  sub_int128 ~x0:a ~x1:b ~y0:c ~y1:c
let[@inline never] sub_abcd a b c d =
  sub_int128 ~x0:a ~x1:b ~y0:c ~y1:d

let add_reference ~x0 ~x1 ~y0 ~y1 =
  let z0 = I64.add x0 y0 in
  let carry = if I64.unsigned_compare z0 x0 < 0 then #1L else #0L in
  #(z0, I64.add (I64.add x1 y1) carry)

let sub_reference ~x0 ~x1 ~y0 ~y1 =
  let borrow = if I64.unsigned_compare x0 y0 < 0 then #1L else #0L in
  #(I64.sub x0 y0, I64.sub (I64.sub x1 y1) borrow)

let check_shared name reference tests =
  let values =
    [ 0L; 1L; -1L; 5L; 100L; 0x7fff_ffff_ffff_ffffL; 0x8000_0000_0000_0000L ]
  in
  let each f = List.iter (fun v -> f (I64.of_int64 v)) values in
  List.iter
    (fun (pattern, f) ->
      each (fun a ->
          each (fun b ->
              each (fun c ->
                  each (fun d ->
                      let pick i =
                        match pattern.[i] with
                        | 'a' -> a
                        | 'b' -> b
                        | 'c' -> c
                        | _ -> d
                      in
                      let #(z0, z1) = f a b c d in
                      let #(r0, r1) =
                        reference ~x0:(pick 0) ~x1:(pick 1) ~y0:(pick 2)
                          ~y1:(pick 3)
                      in
                      if not (I64.equal z0 r0 && I64.equal z1 r1)
                      then
                        Printf.printf
                          "%s_%s %Lx %Lx %Lx %Lx: (%Lx,%Lx) <> (%Lx,%Lx)\n" name
                          pattern (I64.to_int64 a) (I64.to_int64 b)
                          (I64.to_int64 c) (I64.to_int64 d) (I64.to_int64 z0)
                          (I64.to_int64 z1) (I64.to_int64 r0)
                          (I64.to_int64 r1))))))
    tests

let () =
  check_shared "add" add_reference
    [ "aaaa", add_aaaa;
      "aaab", add_aaab;
      "aaba", add_aaba;
      "aabb", add_aabb;
      "aabc", add_aabc;
      "abaa", add_abaa;
      "abab", add_abab;
      "abac", add_abac;
      "abba", add_abba;
      "abbb", add_abbb;
      "abbc", add_abbc;
      "abca", add_abca;
      "abcb", add_abcb;
      "abcc", add_abcc;
      "abcd", add_abcd ]

let () =
  check_shared "sub" sub_reference
    [ "aaaa", sub_aaaa;
      "aaab", sub_aaab;
      "aaba", sub_aaba;
      "aabb", sub_aabb;
      "aabc", sub_aabc;
      "abaa", sub_abaa;
      "abab", sub_abab;
      "abac", sub_abac;
      "abba", sub_abba;
      "abbb", sub_abbb;
      "abbc", sub_abbc;
      "abca", sub_abca;
      "abcb", sub_abcb;
      "abcc", sub_abcc;
      "abcd", sub_abcd ]

(* low * low -> (low,high) *)
external mul_int64 : int64_u -> int64_u -> #(int64_u * int64_u) = "" "caml_int64_mul128"
[@@noalloc] [@@builtin]

let () =
  let #(z0,z1) = mul_int64 #2L #3L in
  eq z0 #6L;
  eq z1 #0L

let () = (* -(1<<63) * 2 *)
  let #(z0,z1) = mul_int64 (mini ()) #2L in
  eq z0 #0L;
  eq z1 (-#1L)

let () = (* -(1<<63) * (-1) *)
  let #(z0,z1) = mul_int64 (mini ()) (-#1L) in
  eq z0 (mini ());
  eq z1 #0L

let () = (* (1<<63-1) * -2 *)
  let #(z0,z1) = mul_int64 (maxi ()) (-#2L) in
  eq z0 #2L;
  eq z1 (-#1L)

(* low * low -> (low,high) *)
external unsigned_mul_int64 : int64_u -> int64_u -> #(int64_u * int64_u) = "" "caml_unsigned_int64_mul128"
[@@noalloc] [@@builtin]

let () =
  let #(z0,z1) = unsigned_mul_int64 #2L #3L in
  eq z0 #6L;
  eq z1 #0L

let () = (* 1<<63 * 2 *)
  let #(z0,z1) = unsigned_mul_int64 (mini ()) #2L in
  eq z0 #0L;
  eq z1 #1L

let () = (* (1<<64-1) * 2 *)
  let #(z0,z1) = unsigned_mul_int64 (-#1L) #2L in
  eq z0 I64.(shift_left (-#1L) 1);
  eq z1 #1L

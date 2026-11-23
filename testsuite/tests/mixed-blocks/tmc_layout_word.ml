(* TEST
 include stdlib_upstream_compatible;
 {
   native;
 }{
   bytecode;
 }
*)

(* Tail-modulo-cons into mixed constructor blocks, with and without a
   layout word. A block gets a layout word when its value prefix is
   too long for the header's scan mask (16 or more values). *)

module Float_u = Stdlib_upstream_compatible.Float_u

(* 2 values: no layout word. *)
type short = Snil | Scons of { s0 : string; stl : short; sf : float# }

let[@tail_mod_cons] rec build_short n =
  if n = 0 then Snil
  else Scons { s0 = string_of_int n; stl = build_short (n - 1); sf = #1.0 }

let rec check_short n = function
  | Snil -> n
  | Scons { s0; stl; sf } ->
    assert (s0 = string_of_int n);
    assert (Float_u.to_float sf = 1.0);
    check_short (n - 1) stl

(* 15 values: the longest prefix without a layout word. *)
type mid =
  | Mnil
  | Mcons of
      { m0 : string; m1 : string; m2 : string; m3 : string; m4 : string;
        m5 : string; m6 : string; m7 : string; m8 : string; m9 : string;
        m10 : string; m11 : string; m12 : string; m13 : string;
        mtl : mid; mf : float# }

let[@tail_mod_cons] rec build_mid n =
  if n = 0 then Mnil
  else
    let s = string_of_int n in
    Mcons
      { m0 = s; m1 = s; m2 = s; m3 = s; m4 = s; m5 = s; m6 = s; m7 = s;
        m8 = s; m9 = s; m10 = s; m11 = s; m12 = s; m13 = s;
        mtl = build_mid (n - 1); mf = #1.0 }

let rec check_mid n = function
  | Mnil -> n
  | Mcons { m0; m13; mtl; mf; _ } ->
    assert (m0 = string_of_int n);
    assert (m13 = string_of_int n);
    assert (Float_u.to_float mf = 1.0);
    check_mid (n - 1) mtl

(* 16 values: a layout word, with the hole last in the prefix. *)
type long =
  | Lnil
  | Lcons of
      { v0 : string; v1 : string; v2 : string; v3 : string; v4 : string;
        v5 : string; v6 : string; v7 : string; v8 : string; v9 : string;
        v10 : string; v11 : string; v12 : string; v13 : string;
        v14 : string; tl : long; f : float# }

let[@tail_mod_cons] rec build_long n =
  if n = 0 then Lnil
  else
    let s = string_of_int n in
    Lcons
      { v0 = s; v1 = s; v2 = s; v3 = s; v4 = s; v5 = s; v6 = s; v7 = s;
        v8 = s; v9 = s; v10 = s; v11 = s; v12 = s; v13 = s; v14 = s;
        tl = build_long (n - 1); f = #1.0 }

let rec check_long n = function
  | Lnil -> n
  | Lcons { v0; v14; tl; f; _ } ->
    assert (v0 = string_of_int n);
    assert (v14 = string_of_int n);
    assert (Float_u.to_float f = 1.0);
    check_long (n - 1) tl

(* 16 values: a layout word, with the hole first. *)
type first =
  | Fnil
  | Fcons of
      { ftl : first; f1 : string; f2 : string; f3 : string; f4 : string;
        f5 : string; f6 : string; f7 : string; f8 : string; f9 : string;
        f10 : string; f11 : string; f12 : string; f13 : string;
        f14 : string; f15 : string; ff : float# }

let[@tail_mod_cons] rec build_first n =
  if n = 0 then Fnil
  else
    let s = string_of_int n in
    Fcons
      { ftl = build_first (n - 1); f1 = s; f2 = s; f3 = s; f4 = s; f5 = s;
        f6 = s; f7 = s; f8 = s; f9 = s; f10 = s; f11 = s; f12 = s;
        f13 = s; f14 = s; f15 = s; ff = #1.0 }

let rec check_first n = function
  | Fnil -> n
  | Fcons { ftl; f1; f15; ff; _ } ->
    assert (f1 = string_of_int n);
    assert (f15 = string_of_int n);
    assert (Float_u.to_float ff = 1.0);
    check_first (n - 1) ftl

let () =
  let n = 5 in
  Printf.printf "short: %d left of %d\n" (check_short n (build_short n)) n;
  Printf.printf "mid: %d left of %d\n" (check_mid n (build_mid n)) n;
  Printf.printf "long: %d left of %d\n" (check_long n (build_long n)) n;
  Printf.printf "first: %d left of %d\n" (check_first n (build_first n)) n

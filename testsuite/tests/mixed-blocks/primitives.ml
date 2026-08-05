(* TEST
   include stdlib_stable;
   include stdlib_upstream_compatible;
   ocamlopt_flags="-extension layouts_beta -extension small_numbers";
   (* ocamlopt_flags+=" -dlambda"; *)
*)

let pr = print_endline

module Float32_u = Stdlib_stable.Float32_u
module Float_u = Stdlib_upstream_compatible.Float_u
module Int8_u = Stdlib_stable.Int8_u
module Int16_u = Stdlib_stable.Int16_u
module Int32_u = Stdlib_upstream_compatible.Int32_u
module Int64_u = Stdlib_upstream_compatible.Int64_u
module Nativeint_u = Stdlib_upstream_compatible.Nativeint_u

module Monomorphic_immutable = struct
  type field = float#
  type t = { a : field } [@@boxed]

  external make : field -> t = "%makeblock"
  external field0 : t -> field = "%field0_of_1_immut"

  let print a =
    pr (Float_u.to_string a)

  let construct_by_syntax a = { a }
  let construct_by_primitive a = make a
  let access_by_syntax { a } = print a
  let access_by_primitive t = print (field0 t)

  let () =
    pr "--- Monomorphic_immutable ---";
    let t1 = construct_by_syntax #1.0 in
    access_by_syntax t1;
    access_by_primitive t1;
    let t2 = construct_by_primitive #2.0 in
    access_by_syntax t2;
    access_by_primitive t2
end

module Monomorphic_mutable = struct
  type field = #(float32# * int32#)
  type t = { mutable a : field } [@@boxed]

  external make : field -> t = "%makemutable"
  external field0 : t -> field = "%field0_of_1"
  external setfield0 : t -> field -> unit = "%setfield0_of_1"

  let print #(a1, a2) =
    pr ("#(" ^ Float32_u.to_string a1 ^ ", " ^ Int32_u.to_string a2 ^ ")")

  let construct_by_syntax a = { a }
  let construct_by_primitive a = make a
  let access_by_syntax { a } = print a
  let access_by_primitive t = print (field0 t)
  let modify_by_syntax t =
    let #(a1, a2) = t.a in
    t.a <- #(Float32_u.mul a1 #10.0s, Int32_u.mul a2 #10l)
  let modify_by_primitive t =
    let #(a1, a2) = field0 t in
    setfield0 t #(Float32_u.mul a1 #10.0s, Int32_u.mul a2 #10l)

  let () =
    pr "\n--- Monomorphic_mutable ---";
    let t1 = construct_by_syntax #(#1.0s, #1l) in
    access_by_syntax t1;
    access_by_primitive t1;
    modify_by_syntax t1;
    access_by_primitive t1;
    let t2 = construct_by_primitive #(#2.0s, #2l) in
    access_by_syntax t2;
    access_by_primitive t2;
    modify_by_primitive t2;
    access_by_syntax t2
end

module Polymorphic_immutable = struct
  type ('a : any) t = { a : 'a; } [@@boxed]

  external make : ('a : any). 'a -> 'a t = "%makeblock" [@@layout_poly]
  external field0 : ('a : any). 'a t -> 'a = "%field0_of_1_immut"
  [@@layout_poly]

  type field = int64#

  let print a =
    pr (Int64_u.to_string a)

  let construct_by_syntax (a : field) = { a }
  let construct_by_primitive (a : field) = make a
  (* CR-soon lmaurer: The [field t] ascription shouldn't be necessary. There's a
     bug here. *)
  let access_by_syntax ({ a } : field t) = print a
  let access_by_primitive t = print (field0 t)

  let () =
    pr "\n--- Polymorphic_immutable ---";
    let t1 = construct_by_syntax #1L in
    access_by_syntax t1;
    access_by_primitive t1;
    let t2 = construct_by_primitive #2L in
    access_by_syntax t2;
    access_by_primitive t2
end

module Polymorphic_mutable = struct
  type ('a : any) t = { mutable a : 'a; } [@@boxed]

  external make : ('a : any). 'a -> 'a t = "%makemutable" [@@layout_poly]
  external field0 : ('a : any). 'a t -> 'a = "%field0_of_1" [@@layout_poly]
  external setfield0 : ('a : any). 'a t -> 'a -> unit = "%setfield0_of_1"
  [@@layout_poly]

  type field = #(int8# * unit# * string)

  let print #(a1, #(), a3) =
    pr ("#(" ^ Int8_u.to_string a1 ^ ", #(), " ^ a3 ^ ")")

  let construct_by_syntax (a : field) = { a }
  let construct_by_primitive (a : field) = make a
  (* CR-soon lmaurer: The [field t] ascription shouldn't be necessary. There's a
     bug here. *)
  let access_by_syntax ({ a } : field t) = print a
  let access_by_primitive t = print (field0 t)
  let modify_by_syntax (t : field t) =
    let #(a1, a2, a3) = t.a in
    t.a <- #(Int8_u.mul a1 #10s, a2, a3 ^ " times ten")
  let modify_by_primitive t =
    let #(a1, a2, a3) = field0 t in
    setfield0 t #(Int8_u.mul a1 #10s, a2, a3 ^ " times ten")

  let () =
    pr "\n--- Polymorphic_mutable ---";
    let t1 = construct_by_syntax #(#1s, #(), "one") in
    access_by_syntax t1;
    access_by_primitive t1;
    modify_by_syntax t1;
    access_by_primitive t1;
    let t2 = construct_by_primitive #(#2s, #(), "two") in
    access_by_syntax t2;
    access_by_primitive t2;
    modify_by_primitive t2;
    access_by_syntax t2
end

module Ref = struct
  type field = #(int16# * nativeint#)

  let print #(a1, a2) =
    pr ("#(" ^ Int16_u.to_string a1 ^ ", " ^ Nativeint_u.to_string a2 ^ ")")

  let construct_by_syntax (a : field) = { contents = a }
  let construct_by_primitive (a : field) = ref a
  (* CR-soon lmaurer: The [field ref] ascription shouldn't be necessary. There's
     a bug here. *)
  let access_by_syntax ({ contents } : field ref) = print contents
  let access_by_primitive t = print !t
  let modify_by_syntax (t : field ref) =
    let #(a1, a2) = t.contents in
    t.contents <- #(Int16_u.mul a1 #10S, Nativeint_u.mul a2 #10n)
  let modify_by_primitive t =
    let #(a1, a2) = !t in
    t := #(Int16_u.mul a1 #10S, Nativeint_u.mul a2 #10n)

  let () =
    pr "\n--- Ref ---";
    let t1 = construct_by_syntax #(#1S, #2n) in
    access_by_syntax t1;
    access_by_primitive t1;
    modify_by_syntax t1;
    access_by_primitive t1;
    let t2 = construct_by_primitive #(#3S, #4n) in
    access_by_syntax t2;
    access_by_primitive t2;
    modify_by_primitive t2;
    access_by_syntax t2
end

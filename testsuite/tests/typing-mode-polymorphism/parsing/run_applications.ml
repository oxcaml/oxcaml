(* TEST
 flags += "-extension mode_polymorphism_alpha";
 native;
*)

(* Multi-argument mode-polymorphic signatures with implementations must be
   callable: fully applied, partially applied and with omitted labelled arguments. *)

module type Fst = sig
  val fst : 'a @ [< 'm] -> 'b @ 'n -> 'a @ [> 'm]
end

module M_fst : Fst = struct
  let fst a _ = a
end

let () = assert (M_fst.fst 1 2 = 1)

let call_fst_partial () =
  let (p @ local) = M_fst.fst 3 in
  assert (p 4 = 3);
  assert (p 5 = 3)

let () = call_fst_partial ()

let fst_local () =
  let x = stack_ (1, 2) in
  let (r @ local) = M_fst.fst x 5 in
  match r with a, b -> assert (a = 1 && b = 2)

let () = fst_local ()

let fst_partial_local () =
  let x = stack_ (6, 7) in
  let (p @ local) = M_fst.fst x in
  let (r @ local) = p 8 in
  match r with a, b -> assert (a = 6 && b = 7)

let () = fst_partial_local ()

module type Curried = sig
  val curried :
    'a @ [< 'p & global] ->
    'b @ [< 'o & global] ->
    'c @ [< 'n & global] ->
    'a * 'b * 'c @ [> 'n | 'o | 'p]
end

module M_curried : Curried = struct
  let curried a b c = (a, b, c)
end

let () = assert (M_curried.curried 1 2 3 = (1, 2, 3))

let curried_partial1 = M_curried.curried 1

let curried_partial2 = curried_partial1 2

let () = assert (curried_partial2 3 = (1, 2, 3))

let () = assert (curried_partial1 4 5 = (1, 4, 5))

module type Apply = sig
  val apply :
    ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< global] ->
    'a @ [< 'n] -> 'b @ [> 'm | dynamic]
end

module M_apply : Apply = struct
  let apply f x = f x
end

let () = assert (M_apply.apply (fun x -> x + 1) 1 = 2)

let apply_partial = M_apply.apply (fun x -> x * 2)

let () = assert (apply_partial 21 = 42)

let () = assert (apply_partial 4 = 8)

module type Close_fst = sig
  val fst : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | local once]
end

module M_close_fst : Close_fst = struct
  let fst a _ = a
end

let () = assert (M_close_fst.fst 1 2 = 1)

let close_fst_partial () =
  let (p @ local) = M_close_fst.fst 7 in
  assert (p 8 = 7)

let () = close_fst_partial ()

module type Labelled = sig
  val labelled : a:'a @ [< 'm & global] -> b:'b @ 'n -> 'a @ [> 'm]
end

module M_labelled : Labelled = struct
  let labelled ~a ~b:_ = a
end

let () = assert (M_labelled.labelled ~a:1 ~b:2 = 1)

let () = assert (M_labelled.labelled ~b:2 ~a:1 = 1)

let labelled_partial = M_labelled.labelled ~b:2

let () = assert (labelled_partial ~a:3 = 3)

module type Labelled3 = sig
  val labelled3 :
    a:'a @ [< 'm & global] ->
    b:'b @ [< 'n & global] ->
    c:'c @ [< 'o & global] ->
    'a * 'b * 'c @ [> 'm | 'n | 'o]
end

module M_labelled3 : Labelled3 = struct
  let labelled3 ~a ~b ~c = (a, b, c)
end

let omitted_middle_label = M_labelled3.labelled3 ~a:1 ~c:3

let () = assert (omitted_middle_label ~b:2 = (1, 2, 3))

module type Portable_fst = sig
  val fst : 'a @ [< 'm & portable] -> 'b @ 'n -> 'a @ [> 'm]
end

module M_portable_fst : Portable_fst = struct
  let fst a _ = a
end

let () = assert (M_portable_fst.fst 1 2 = 1)

let call_portable_partial () =
  let (p @ local) = M_portable_fst.fst 9 in
  assert (p 10 = 9)

let () = call_portable_partial ()

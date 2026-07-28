(* TEST
 flags = "-extension layout_poly_alpha";
 expect.opt;
*)

(* Tests for *applications* of static functors: instantiation of the
   [Ltemplate]s that static functors compile to. Definitions alone are
   covered by [apply_layout.ml]; the typing rules by
   [typing-modes/staticity.ml]. Most cases pass a module containing a
   layout-polymorphic identity and instantiate it at two layouts inside the
   functor body, which only compiles if the parameter really is static and
   the instantiation really happened at compile time.

   The toplevel translates each structure item as its own phrase, which
   currently loses the compile-time half of templates across items (see the
   CR-soon in the toplevel-phrase-boundaries section below). Until that is
   fixed, each case is wrapped in a single [let] that defines the functor
   and its argument with [let module] and reads the results back, so
   everything is compiled -- and instantiated -- in one phrase. *)

external to_float : float# -> float = "%box_float"
external to_int64 : int64# -> int64 = "%box_int64"

module type S = sig val y : int end

module type Id = sig
  val id : layout_ l. ('a : l). 'a -> 'a
end

(* Used only by the toplevel-phrase-boundary cases below. *)
module (Id @ static) = struct let poly_ id x = x end
module (A @ static) = struct let y = 1 end
[%%expect{|
external to_float : float# -> float = "%box_float"
external to_int64 : int64# -> int64 = "%box_int64"
module type S = sig val y : int end
module type Id = sig val id : layout_ l. ('a : l). 'a -> 'a end
module Id : sig val id : la`yout_ l. ('a : l). 'a -> 'a end
module A : sig val y : int end
|}]

(* 1. Sanity: static functors over plain modules. *)

let r1 =
  let module F (X : S @ static) = struct let z = X.y + 1 end in
  let module R = F (struct let y = 1 end) in
  R.z
[%%expect{|
val r1 : int = 2
|}]

(* The body also uses a plain value from the enclosing scope. *)
let r2 =
  let k = 10 in
  let module F (X : S @ static) = struct let z = X.y + k end in
  let module R = F (struct let y = 2 end) in
  R.z
[%%expect{|
val r2 : int = 12
|}]

(* 2. One static argument with a layout-polymorphic field. *)

let (r3i, r3f) =
  let module IdA = struct let poly_ id x = x end in
  let module F (M : Id @ static) = struct
    let i = M.id 42
    let f = M.id #1.0
  end in
  let module R = F (IdA) in
  (R.i, to_float R.f)
[%%expect{|
val r3i : int = 42
val r3f : float = 1.
|}]

(* Applied to an inline structure rather than a named module. *)
let (r4i, r4f) =
  let module F (M : Id @ static) = struct
    let i = M.id 42
    let f = to_float (M.id #1.5)
  end in
  let module R = F (struct let poly_ id x = x end) in
  (R.i, R.f)
[%%expect{|
val r4i : int = 42
val r4f : float = 1.5
|}]

(* The same functor applied twice: two instantiations of one template,
   distinguished by a constant in the argument. *)
module type IdC = sig
  val c : int
  val id : layout_ l. ('a : l). 'a -> 'a
end
let (r5, r5') =
  let module F (M : IdC @ static) = struct
    let i = M.id M.c
    let f = to_float (M.id #1.0)
  end in
  let module R = F (struct let c = 1 let poly_ id x = x end) in
  let module R' = F (struct let c = 2 let poly_ id x = x end) in
  (R.i, R'.i)
[%%expect{|
module type IdC = sig val c : int val id : layout_ l. ('a : l). 'a -> 'a end
val r5 : int = 1
val r5' : int = 2
|}]

(* A static parameter the body never uses: the contrast case for the capture
   cases below. *)
let r6 =
  let module F (M : Id @ static) = struct let z = 9 end in
  let module R = F (struct let poly_ id x = x end) in
  R.z
[%%expect{|
val r6 : int = 9
|}]

(* Result coercion at the definition: the parser nests the constraint inside
   the functor, so the coercion lands inside the template body. *)
let r7 =
  let module F (M : Id @ static) : sig val i : int end = struct
    let i = M.id 5
    let extra = "dropped"
  end in
  let module R = F (struct let poly_ id x = x end) in
  R.i
[%%expect{|
val r7 : int = 5
|}]

(* Result coercion at the use site: [apply_coercion] wraps the
   instantiation. *)
let r8 =
  let module F (M : Id @ static) = struct
    let i = M.id 6
    let extra = "dropped"
  end in
  let module R : sig val i : int end = F (struct let poly_ id x = x end) in
  R.i
[%%expect{|
val r8 : int = 6
|}]

(* Argument with extra fields: the argument gets a structure coercion. *)
let r9 =
  let module F (M : Id @ static) = struct let i = M.id 7 end in
  let module Big = struct
    let poly_ id x = x
    let unrelated = "extra"
  end in
  let module R = F (Big) in
  R.i
[%%expect{|
val r9 : int = 7
|}]

(* The argument is itself a static functor application. [Wrap] needs a
   functor-type ascription declaring its return static; an inline result
   annotation does not do that (see the typing negatives below). *)
let (r10i, r10f) =
  let module Wrap : functor (M : Id @ static) -> Id @ static =
    functor (M : Id @ static) -> struct let poly_ id = M.id end
  in
  let module F (M : Id @ static) = struct
    let i = M.id 8
    let f = to_float (M.id #8.0)
  end in
  let module R = F (Wrap (struct let poly_ id x = x end)) in
  (R.i, R.f)
[%%expect{|
val r10i : int = 8
val r10f : float = 8.
|}]

(* A static functor applied inside another static functor's body. *)
let (r11i, r11f) =
  let module F (M : Id @ static) = struct
    let i = M.id 10
    let f = to_float (M.id #10.0)
  end in
  let module Outer (M : Id @ static) = struct
    module R = F (M)
    let i = R.i
    let f = R.f
  end in
  let module R = Outer (struct let poly_ id x = x end) in
  (R.i, R.f)
[%%expect{|
val r11i : int = 10
val r11f : float = 10.
|}]

(* Immediate application of an anonymous functor. *)
let r12 =
  let module R =
    (functor (M : Id @ static) -> struct let i = M.id 11 end)
      (struct let poly_ id x = x end)
  in
  R.i
[%%expect{|
val r12 : int = 11
|}]

(* 3. Captured values: the [tmpl_env] path. Each case uses what it captures
   and reads the result back, so a wrong environment index or block shape
   shows up as a wrong value rather than a crash. *)

(* Values of several layouts at once, interleaved. *)
let (k1i, k1f, k1s, k1i64) =
  let ci = 17 in
  let cf = #2.5 in
  let cs = "cap" in
  let ci64 = #3L in
  let module K (M : Id @ static) = struct
    let i = M.id ci
    let f = M.id cf
    let s = M.id cs
    let i64 = M.id ci64
  end in
  let module R = K (struct let poly_ id x = x end) in
  (R.i, to_float R.f, R.s, to_int64 R.i64)
[%%expect{|
val k1i : int = 17
val k1f : float = 2.5
val k1s : string = "cap"
val k1i64 : int64 = 3L
|}]

(* Capture a module and use a field of it. *)
let k2 =
  let module CM = struct let v = 21 end in
  let module K (M : Id @ static) = struct
    let i = M.id CM.v
  end in
  let module R = K (struct let poly_ id x = x end) in
  R.i
[%%expect{|
val k2 : int = 21
|}]

(* Capture a layout-polymorphic value bound outside the functor and use it
   at two layouts inside: a kind template in the template's environment. *)
let (k3i, k3f) =
  let poly_ myid x = x in
  let module K (M : Id @ static) = struct
    let i = myid 31
    let f = to_float (myid #31.0)
  end in
  let module R = K (struct let poly_ id x = x end) in
  (R.i, R.f)
[%%expect{|
val k3i : int = 31
val k3f : float = 31.
|}]

(* Capture the outer functor's parameter from an inner, unmerged functor:
   the inner template's environment holds the outer parameter. *)
let (k4i, k4f) =
  let module Outer (M : Id @ static) = struct
    module Inner (N : Id @ static) = struct
      let i = M.id 41
      let f = to_float (N.id #41.0)
    end
  end in
  let module Half = Outer (struct let poly_ id x = x end) in
  let module R = Half.Inner (struct let poly_ id x = x end) in
  (R.i, R.f)
[%%expect{|
val k4i : int = 41
val k4f : float = 41.
|}]

(* The argument module captures. *)
module type IdK = sig
  val k : int
  val id : layout_ l. ('a : l). 'a -> 'a
end
let k5 =
  let n = 51 in
  let module K (M : IdK @ static) = struct
    let i = M.id M.k
  end in
  let module R = K (struct let poly_ id x = x let k = n end) in
  R.i
[%%expect{|
module type IdK = sig val k : int val id : layout_ l. ('a : l). 'a -> 'a end
val k5 : int = 51
|}]

(* A class defined inside the static functor body: the class table bound by
   the enclosing [oo_wrap] and the unit-level shared constant land in the
   template's environment. *)
let k6 =
  let module K (M : Id @ static) = struct
    class c = object method v = M.id 61 end
    let i = (new c)#v
  end in
  let module R = K (struct let poly_ id x = x end) in
  R.i
[%%expect{|
>> Fatal error: slambda eval: unexpected missing value
Uncaught exception: Misc.Fatal_error

|}]

(* Captures used through the parameter's [id] under a result coercion, so
   capture and coercion interact. *)
let (k7i, k7f) =
  let cc1 = 71 in
  let cc2 = #7.5 in
  let module K (M : Id @ static) : sig val i : int val f : float end = struct
    let i = M.id cc1
    let f = to_float (M.id cc2)
    let extra = "dropped"
  end in
  let module R = K (struct let poly_ id x = x end) in
  (R.i, R.f)
[%%expect{|
val k7i : int = 71
val k7f : float = 7.5
|}]

(* 4. A static parameter followed by a dynamic one: one merged template; the
   static application is an instantiation, the dynamic one an ordinary
   application of its result. *)

let (d1i, d1f) =
  let module F (M : Id @ static) (X : S) = struct
    let i = M.id X.y
    let f = to_float (M.id #4.0)
  end in
  let module R = F (struct let poly_ id x = x end) (struct let y = 1 end) in
  (R.i, R.f)
[%%expect{|
val d1i : int = 1
val d1f : float = 4.
|}]

(* Stepwise: the partial application bound on its own. *)
let d2 =
  let module F (M : Id @ static) (X : S) = struct
    let i = M.id X.y
  end in
  let module Half = F (struct let poly_ id x = x end) in
  let module R = Half (struct let y = 2 end) in
  R.i
[%%expect{|
val d2 : int = 2
|}]

(* A generative second parameter. *)
let d3 =
  let module F (M : Id @ static) () = struct
    let i = M.id 44
  end in
  let module R = F (struct let poly_ id x = x end) () in
  R.i
[%%expect{|
val d3 : int = 44
|}]

(* 5. Two static parameters that do not merge: the second instantiation must
   find a template in the compile-time half of the first one's result. *)

(* A result signature on the functor defeats merging: the parser nests the
   constraint inside the outer functor. Applied stepwise; a one-go
   application would need the return staticity declared by a functor-type
   ascription, which puts the constraint at the binding and lets the
   functors merge again. *)
let (u1i, u1f) =
  let module U (M : Id @ static) :
    (functor (N : Id @ static) -> sig val i : int val f : float end)
      @ static =
    functor (N : Id @ static) -> struct
      let i = M.id 1
      let f = to_float (N.id #2.0)
    end
  in
  let module Half = U (struct let poly_ id x = x end) in
  let module R = Half (struct let poly_ id x = x end) in
  (R.i, R.f)
[%%expect{|
val u1i : int = 1
val u1f : float = 2.
|}]

(* Returning a named functor: the outer body is a module identifier. *)
let u2 =
  let module Inner (N : Id @ static) = struct let i = N.id 3 end in
  let module U (M : Id @ static) = Inner in
  let module Half = U (struct let poly_ id x = x end) in
  let module R = Half (struct let poly_ id x = x end) in
  R.i
[%%expect{|
val u2 : int = 3
|}]

(* The same, applied in one go: the functor-type ascription declares the
   return static while the body stays a module identifier, so nothing
   merges. *)
let u3 =
  let module Inner (N : Id @ static) = struct let i = N.id 4 end in
  let module U : functor (M : Id @ static)
    -> (functor (N : Id @ static) -> sig val i : int end) @ static =
    functor (M : Id @ static) -> Inner
  in
  let module R =
    U (struct let poly_ id x = x end) (struct let poly_ id x = x end)
  in
  R.i
[%%expect{|
val u3 : int = 4
|}]

(* 6. Two static parameters, merged. *)

let g1 =
  let module G (M : Id @ static) (N : Id @ static) = struct
    let i = M.id (N.id 6)
  end in
  let module Half = G (struct let poly_ id x = x end) in
  let module R = Half (struct let poly_ id x = x end) in
  R.i
[%%expect{|
Uncaught exception: Invalid_argument("Misc.Stdlib.Array.fold_left2")

|}]

(* One-go application, with the intermediate declared static via a
   functor-type ascription (the body stays syntactically nested functors,
   so they still merge). *)
let g2 =
  let module G : functor (M : Id @ static)
    -> (functor (N : Id @ static) -> sig val i : int end) @ static =
    functor (M : Id @ static) (N : Id @ static) -> struct
      let i = M.id (N.id 7)
    end
  in
  let module R =
    G (struct let poly_ id x = x end) (struct let poly_ id x = x end)
  in
  R.i
[%%expect{|
Uncaught exception: Invalid_argument("Misc.Stdlib.Array.fold_left2")

|}]

(* 7. Toplevel phrase boundaries. *)

module F_top (X : S @ static) = struct let z = X.y + 1 end
module R_top = F_top (A)
let r_top = R_top.z
[%%expect{|
module F_top : functor (X : S) -> sig val z : int end
>> Fatal error: slambda eval: unexpected missing value
Uncaught exception: Misc.Fatal_error

|}]

(* Instantiation in a later phrase than the definition. *)
module F_later (M : Id @ static) = struct let i = M.id 12 end
[%%expect{|
module F_later : functor (M : Id) -> sig val i : int end
|}]

module R_later = F_later (Id)
let r_later = R_later.i
[%%expect{|
>> Fatal error: slambda eval: unexpected missing value
Uncaught exception: Misc.Fatal_error

|}]

(* A toplevel module as the argument of a local instantiation: the
   argument's compile-time half crosses a phrase boundary. *)
let r_toparg =
  let module F (M : Id @ static) = struct let i = M.id 1 end in
  let module R = F (Id) in
  R.i
[%%expect{|
>> Fatal error: slambda eval: unexpected missing value
Uncaught exception: Misc.Fatal_error

|}]

(* An unannotated toplevel module binding still satisfies a static parameter
   at typing. *)
module Unannotated = struct let y = 3 end
module R_unannotated = F_top (Unannotated)
[%%expect{|
module Unannotated : sig val y : int end
>> Fatal error: slambda eval: unexpected missing value
Uncaught exception: Misc.Fatal_error

|}]

(* A layout-polymorphic value bound by an earlier toplevel item is dynamic,
   like persistent modules (cf. the CR-soon in typing-modes/staticity.ml),
   so it cannot be instantiated inside the functor body. *)
let poly_ myid x = x
module K_dyn (M : Id @ static) = struct
  let i = myid 31
end
[%%expect{|
val myid : layout_ l. ('a : l). 'a -> 'a = <lpoly>
Line 3, characters 10-14:
3 |   let i = myid 31
              ^^^^
Error: The value "myid" is "dynamic"
       but is expected to be "static"
         because it is layout-polymorphic and being instantiated here.
|}]

(* Likewise a value from an earlier item makes an argument field dynamic. *)
let n = 51
module K_arg (M : IdK @ static) = struct
  let i = M.id M.k
end
module KR_arg = K_arg (struct let poly_ id x = x let k = n end)
[%%expect{|
val n : int = 51
module K_arg : functor (M : IdK) -> sig val i : int end
Line 5, characters 16-63:
5 | module KR_arg = K_arg (struct let poly_ id x = x let k = n end)
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modules do not match:
       sig val id : layout_ l. ('a : l). 'a -> 'a val k : int @@ dynamic end @ static
     is not included in IdK @ static
     Values do not match:
       val k : int @@ dynamic (* in a structure at static *)
     is not included in
       val k : int (* in a structure at static *)
     The first is "dynamic"
     but the second is "static".
|}]

(* 8. Typing negatives. *)

(* An inline result annotation does not declare the return staticity: the
   application's result stays dynamic and cannot feed a static parameter
   (contrast with the ascribed [Wrap] case above). *)
module WrapInline (M : Id @ static) : Id @ static = struct
  let poly_ id = M.id
end
module F_wrap (M : Id @ static) = struct let i = M.id 8 end
module R_wrap = F_wrap (WrapInline (Id))
[%%expect{|
module WrapInline : functor (M : Id) -> Id
module F_wrap : functor (M : Id) -> sig val i : int end
Line 5, characters 16-40:
5 | module R_wrap = F_wrap (WrapInline (Id))
                    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modules do not match:
       sig val id : layout_ l. ('a : l). 'a -> 'a end @ dynamic
     is not included in Id @ static Got "dynamic" but expected "static".
|}]

(* On a plainly-defined merged functor the intermediate application result
   is dynamic, so the one-go application is a type error. *)
module G_plain (M : Id @ static) (N : Id @ static) = struct
  let i = M.id (N.id 5)
end
module GR_plain = G_plain (Id) (Id)
[%%expect{|
module G_plain : functor (M : Id) (N : Id) -> sig val i : int end
Line 4, characters 18-30:
4 | module GR_plain = G_plain (Id) (Id)
                      ^^^^^^^^^^^^
Error: The functor is "dynamic"
       but is expected to be "static"
         because it shares the staticity of a functor parameter
         which is expected to be "static".
|}]

(* A dynamic parameter followed by a static one: the definition compiles
   (this is the shape the merging prefix guard exists for), but the second
   application requires the intermediate functor to be static. *)
module N_mixed (X : S) (M : Id @ static) = struct
  let i = M.id X.y
end
[%%expect{|
module N_mixed : functor (X : S) (M : Id) -> sig val i : int end
|}]

module NR_mixed = N_mixed (A) (Id)
[%%expect{|
Line 1, characters 18-29:
1 | module NR_mixed = N_mixed (A) (Id)
                      ^^^^^^^^^^^
Error: The functor is "dynamic"
       but is expected to be "static"
         because it shares the staticity of a functor parameter
         which is expected to be "static".
|}]

module NHalf_mixed = N_mixed (A)
[%%expect{|
module NHalf_mixed : functor (M : Id) -> sig val i : int end
|}]

(* Passing a dynamic module (the result of a dynamic functor application) to
   a static parameter. *)
module MkDyn (X : S) = struct let poly_ id x = x end
module Dyn = MkDyn (A)
module F_dyn (M : Id @ static) = struct let i = M.id 9 end
module NR_dyn = F_dyn (Dyn)
[%%expect{|
module MkDyn :
  functor (X : S) -> sig val id : layout_ l. ('a : l). 'a -> 'a end
module Dyn : sig val id : layout_ l. ('a : l). 'a -> 'a end
module F_dyn : functor (M : Id) -> sig val i : int end
Line 4, characters 16-27:
4 | module NR_dyn = F_dyn (Dyn)
                    ^^^^^^^^^^^
Error: Modules do not match:
       sig val id : layout_ l. ('a : l). 'a -> 'a end @ dynamic
     is not included in Id @ static Got "dynamic" but expected "static".
|}]

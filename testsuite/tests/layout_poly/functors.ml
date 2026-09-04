(* TEST
 flags = "-extension layout_poly_alpha";
 expect.opt;
*)

(* Tests for *applications* of static functors: instantiation of the
   [Ltemplate]s that static functors compile to.

   The toplevel translates each structure item as its own phrase, which
   currently loses the compile-time half of templates across items. *)

external to_float : float# -> float = "%box_float"
external to_int64 : int64# -> int64 = "%box_int64"

module type S = sig val y : int end

module type Id = sig
  val id : layout_ l. ('a : l). 'a -> 'a
end

[%%expect{|
external to_float : float# -> float = "%box_float"
external to_int64 : int64# -> int64 = "%box_int64"
module type S = sig val y : int end
module type Id = sig val poly_ id : 'a -> 'a end
|}]

(* Static functors using dynamic data. *)

let r1 =
  let module F (X : S @ static) = struct let z = X.y + 1 end in
  let module R = F (struct let y = 1 end) in
  R.z
[%%expect{|
val r1 : int = 2
|}]

(* Capture dynamic value from the enclosing scope. *)
let r2 =
  let k = 10 in
  let module F (X : S @ static) = struct let z = X.y + k end in
  let module R = F (struct let y = 2 end) in
  R.z
[%%expect{|
val r2 : int = 12
|}]

(* Static functors using static data. *)

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
module type IdC = sig val c : int val poly_ id : 'a -> 'a end
val r5 : int = 1
val r5' : int = 2
|}]

(* The same functor applied twice to the *same* argument, the static part is
   memoized but the dynamic part should run twice. *)
let (r5c, r5m, r5m') =
  let counter = ref 0 in
  let module M = struct let poly_ id x = x end in
  let module F (M : Id @ static) = struct
    let _ = incr counter
    let i = M.id 1
    let f = to_float (M.id #2.0)
  end in
  let module R1 = F (M) in
  let module R2 = F (M) in
  (!counter, R1.i + R2.i, R1.f +. R2.f)
[%%expect{|
val r5c : int = 2
val r5m : int = 2
val r5m' : float = 4.
|}]

(* A static parameter the body never uses. *)
let r6 =
  let module F (M : Id @ static) = struct let z = 9 end in
  let module R = F (struct let poly_ id x = x end) in
  R.z
[%%expect{|
val r6 : int = 9
|}]

(* Result coercion at the definition. *)
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

(* Result coercion at the use site. *)
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

(* Argument coercion. *)
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

(* Functor coercion. *)
let c1 =
  let module Inner (N : Id @ static) = struct
    let i = N.id 4
    let extra = 5
  end in
  let module U : functor (N : Id @ static) -> sig val i : int end = Inner in
  let module R = U (struct let poly_ id x = x end) in
  R.i
[%%expect{|
>> Fatal error: slambda eval: unexpected missing value
Uncaught exception: Misc.Fatal_error

|}]

(* The functor is aliased before being applied. *)
let r9a =
  let module F (M : Id @ static) = struct let i = M.id 7 end in
  let module G = F in
  let module R = G (struct let poly_ id x = x end) in
  R.i
[%%expect{|
val r9a : int = 7
|}]

(* The argument is itself a static functor application. *)
let (r10i, r10f) =
  let module Wrap (M : Id @ static) = struct let poly_ id = M.id end in
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

(* Captured values of several layouts at once. *)
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
module type IdK = sig val k : int val poly_ id : 'a -> 'a end
val k5 : int = 51
|}]

(* A class defined inside the static functor body: the class table bound by
   the enclosing [oo_wrap] and the unit-level shared constant land in the
   template's environment. It also does some weird projection that prevents
   static eval from seeing what's going on. *)
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

(* A static parameter followed by a dynamic one. *)

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

(* Nested static functors (these don't get merged). *)
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

(* Two static parameters. *)
let g1 =
  let module G (M : Id @ static) (N : Id @ static) = struct
    let i = M.id (N.id 6)
  end in
  let module Half = G (struct let poly_ id x = x end) in
  let module R = Half (struct let poly_ id x = x end) in
  R.i
[%%expect{|
>> Fatal error: Slambda eval doesn't support partial or over application of functors.
Uncaught exception: Misc.Fatal_error

|}]

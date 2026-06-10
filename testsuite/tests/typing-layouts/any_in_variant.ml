(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type ('a : any) t = Nope | Yeah of 'a
[%%expect{|
type ('a : any) t = Nope | Yeah of 'a
|}]

let to_option t = match t with Yeah a -> Some a | Nope -> None
[%%expect{|
val to_option : 'a t -> 'a option = <fun>
|}]

(* CR-someday: Actually this one would be reasonable to allow, since we need
   only check the tag. *)
let is_yeah (type a : any) (t : a t) =
  match t with Yeah _ -> true | Nope -> false
[%%expect{|
Line 2, characters 20-21:
2 |   match t with Yeah _ -> true | Nope -> false
                        ^
Error: Constructor arguments being projected must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a constructor argument being projected.
|}]

let to_option (t : int t) = match t with Yeah a -> Some a | Nope -> None
[%%expect{|
val to_option : int t -> int option = <fun>
|}]

let to_option (type a : value) (t : a t) =
  match t with Yeah a -> Some a | Nope -> None
[%%expect{|
val to_option : 'a t -> 'a option = <fun>
|}]

let is_yeah (t : int64# t) = match t with Yeah a -> true | Nope -> false
[%%expect{|
val is_yeah : int64# t -> bool = <fun>
|}]

let is_yeah (type a : bits64) (t : a t) =
  match t with Yeah _ -> true | Nope -> false
[%%expect{|
val is_yeah : ('a : bits64). 'a t -> bool = <fun>
|}]

let of_option o =
  match o with Some a -> Yeah a | None -> Nope
[%%expect{|
val of_option : 'a option -> 'a t = <fun>
|}]

let nope = Nope
[%%expect{|
val nope : 'a t = Nope
|}]

let nope : 'a. 'a t = Nope
[%%expect{|
val nope : 'a t = Nope
|}]

let yeah a = Yeah a
[%%expect{|
val yeah : 'a -> 'a t = <fun>
|}]

let yeah (a : int) = Yeah a
[%%expect{|
val yeah : int -> int t = <fun>
|}]

let yeah a : int t = Yeah a
[%%expect{|
val yeah : int -> int t = <fun>
|}]

let yeah (type a : value) (a : a) = Yeah a
[%%expect{|
val yeah : 'a -> 'a t = <fun>
|}]

let yeah (type a : value) a : a t = Yeah a
[%%expect{|
val yeah : 'a -> 'a t = <fun>
|}]

let yeah (a : int64#) = Yeah a
[%%expect{|
val yeah : int64# -> int64# t = <fun>
|}]

let yeah (type a : bits64) (a : a) = Yeah a
[%%expect{|
val yeah : ('a : bits64). 'a -> 'a t = <fun>
|}]

let yeah (type a : bits64) a : a t = Yeah a
[%%expect{|
val yeah : ('a : bits64). 'a -> 'a t = <fun>
|}]

(* Test that typing and genprintval work when the actual type has kind value *)
let test_block_with_value = Yeah 1
[%%expect {|
val test_block_with_value : int t = Yeah 1
|}]

let test_block = Yeah #1L
[%%expect {|
val test_block : int64# t = Yeah <abstr>
|}]

type ('a : any) any_list = [] | (::) of 'a * 'a any_list

let rec map_unboxed_pair
          (l : #('a * 'b) any_list)
          ~(f : #('a * 'b) -> #('c * 'd)) =
  match l with
  | [] -> []
  | a :: l' -> f a :: map_unboxed_pair l' ~f

let rec box_all (l : #('a * 'b) any_list) : ('a * 'b) any_list =
  match l with
  | [] -> []
  | #(a, b) :: l' -> (a, b) :: box_all l'

let test =
  [#(1, 2); #(3, 4); #(5, 6)]
  |> map_unboxed_pair ~f:(fun #(a, b) -> #(a + 1, b + 2))
  |> box_all

[%%expect {|
type ('a : any) any_list = [] | (::) of 'a * 'a any_list
val map_unboxed_pair :
  #('a * 'b) any_list -> f:(#('a * 'b) -> #('c * 'd)) -> #('c * 'd) any_list =
  <fun>
val box_all : #('a * 'b) any_list -> ('a * 'b) any_list = <fun>
val test : (int * int) any_list =
  (::) ((2, 4), (::) ((4, 6), (::) ((6, 8), [])))
|}]

module All_void_in_module = struct
  type (_ : any, _ : any) type_equal =
    | T : ('a : any). ('a, 'a) type_equal

  module M : sig
    type v : any
    (* annotate [v] as [void] and the program works *)
    type t = A of v | B
    (* outside the module, [A] is a block *)
    val a : t
    val expose : (v, unit#) type_equal
  end = struct
    type v = unit#
    (* inside the module, [A] is an immediate *)
    type t = A of v | B
    let a = A #()
    let expose = T
  end

  let () =
    let T = M.expose in
    match M.a with
    | A _ -> print_endline "ok"
    | B -> assert false
end

(* CR rtjoa for lmaurer: tweaked this wording *)
[%%expect {|
Lines 12-18, characters 8-5:
12 | ........struct
13 |     type v = unit#
14 |     (* inside the module, [A] is an immediate *)
15 |     type t = A of v | B
16 |     let a = A #()
17 |     let expose = T
18 |   end
Error: Signature mismatch:
       Modules do not match:
         sig
           type v = unit#
           type t = A of v | B
           val a : t
           val expose : ('a : any). ('a, 'a) type_equal
         end
       is not included in
         sig
           type v : any
           type t = A of v | B
           val a : t
           val expose : (v, unit#) type_equal
         end
       Type declarations do not match:
         type t = A of v | B
       is not included in
         type t = A of v | B
       Constructors do not match:
         "A of v"
       is not the same as:
         "A of v"
       The first has a fixed representation and the second doesn't.
       Hint: Is there a type that has a representable layout in the first
         but has layout any in the second?
|}]


module M : sig
  type pt = #{ x : int; y : int }
  and t = A of pt
end = struct
  type pt = #{ x : int; y : int }
  type t = A of pt
end
[%%expect{|
module M : sig type pt = #{ x : int; y : int; } and t = A of pt end
|}]

module M : sig
  type pt = #{ x : unit#; y : unit# }
  and t = A of pt
end = struct
  type pt = #{ x : unit#; y : unit# }
  type t = A of pt
end
[%%expect{|
module M : sig type pt = #{ x : unit#; y : unit#; } and t = A of pt end
|}]

(* The contained type may also be the unboxed version of a boxed record. *)
module M : sig
  type pt = { x : int; y : int }
  and t = A of pt#
end = struct
  type pt = { x : int; y : int }
  type t = A of pt#
end
[%%expect{|
module M : sig type pt = { x : int; y : int; } and t = A of pt# end
|}]

(******************************)
(* Requiring representability *)

(* Uses of constructors require the type of the variant to determine
   representable layouts for *all* constructors, because which constructors are
   constant, and hence the runtime tags of all constructors, will depend on
   whether arguments of layout [any] are instantiated to voids. *)

(* An unannotated use is constrained to a sort, which defaults to value *)
let q = Nope
let bad = (q : unit# t)
[%%expect{|
val q : 'a t = Nope
Line 2, characters 11-12:
2 | let bad = (q : unit# t)
               ^
Error: This expression has type "'a t" but an expression was expected of type
         "unit# t"
       The layout of unit# is void
         because it is the unboxed version of the primitive type unit.
       But the layout of unit# must be a value layout
         because of the definition of q at line 1, characters 8-12.
|}]

let ok =
  let q = Nope in
  (q : unit# t)
[%%expect{|
val ok : unit# t = Nope
|}]

(* Uses at determined instantiations are fine, void or not. *)
let nope_value : int t = Nope
let nope_void : unit# t = Nope
let nope_bits : int64# t = Nope
[%%expect{|
val nope_value : int t = Nope
val nope_void : unit# t = Nope
val nope_bits : int64# t = Nope
|}]

(* Error if the representation cannot be representable *)
let is_nope (type a : any) (t : a t) =
  match t with Nope -> true | _ -> false
[%%expect{|
Line 2, characters 15-19:
2 |   match t with Nope -> true | _ -> false
                   ^^^^
Error: The representation of the constructor "Nope"
       depends on the layout of the argument of constructor "Yeah",
       which this instantiation of the type a t does not determine.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a constructor argument being projected.
|}]

let nope (type a : any) () : a t = Nope
[%%expect{|
Line 1, characters 35-39:
1 | let nope (type a : any) () : a t = Nope
                                       ^^^^
Error: The representation of the constructor "Nope"
       depends on the layout of the argument of constructor "Yeah",
       which this instantiation of the type a t does not determine.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a constructor argument being assigned a value.
|}]

type abstract_any : any
let bad = (Nope : abstract_any t)
[%%expect{|
type abstract_any : any
Line 2, characters 11-15:
2 | let bad = (Nope : abstract_any t)
               ^^^^
Error: The representation of the constructor "Nope"
       depends on the layout of the argument of constructor "Yeah",
       which this instantiation of the type abstract_any t does not determine.
       The layout of abstract_any is any
         because of the definition of abstract_any at line 1, characters 0-23.
       But the layout of abstract_any must be representable
         because it's the type of a constructor argument being assigned a value.
|}]

(* Matching with a constant-constructor pattern likewise constrains the
   instantiation to a sort. *)
let _ = (fun x -> match x with Nope -> true | _ -> false) (Yeah #1L)
[%%expect{|
- : bool = false
|}]

let b =
  let f x = match x with Nope -> true | _ -> false in
  f (Yeah #1L)
[%%expect{|
val b : bool = false
|}]

(* Although at the top level it defaults to value *)
let f x = match x with Nope -> true | _ -> false
let bad = f (Yeah #1L)
[%%expect{|
val f : 'a t -> bool = <fun>
Line 2, characters 18-21:
2 | let bad = f (Yeah #1L)
                      ^^^
Error: This expression has type "int64#" but an expression was expected of type
         "('a : value_or_null)"
       The layout of int64# is bits64
         because it is the unboxed version of the primitive type int64.
       But the layout of int64# must be a value layout
         because of the definition of f at line 1, characters 6-48.
|}]

(* The requirement applies to all of the variant's constructors, not just
   constant ones: every constructor's tag depends on the variant's
   representation. *)
type ('a : any, 'b : any) two = T1 of 'a | T2 of 'b | T3 of int

let ok = (T3 5 : (unit#, int) two)
[%%expect{|
type ('a : any, 'b : any) two = T1 of 'a | T2 of 'b | T3 of int
val ok : (unit#, int) two = T3 5
|}]

let bad (type a : any) () : (a, int) two = T3 5
[%%expect{|
Line 1, characters 43-47:
1 | let bad (type a : any) () : (a, int) two = T3 5
                                               ^^^^
Error: The representation of the constructor "T3"
       depends on the layout of the argument of constructor "T1",
       which this instantiation of the type (a, int) two does not determine.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a constructor argument being assigned a value.
|}]

(* An unannotated use gets sorts that default to value. *)
let y = T3 5
[%%expect{|
val y : ('a, 'b) two = T3 5
|}]
let bad = (y : (unit#, int) two)
[%%expect{|
Line 1, characters 11-12:
1 | let bad = (y : (unit#, int) two)
               ^
Error: This expression has type "('a, 'b) two"
       but an expression was expected of type "(unit#, int) two"
       The layout of unit# is void
         because it is the unboxed version of the primitive type unit.
       But the layout of unit# must be a value layout
         because of the definition of y at line 1, characters 8-12.
|}]

let match_t3 x = match x with T3 n -> n | _ -> 0
[%%expect{|
val match_t3 : ('a, 'b) two -> int = <fun>
|}]

let match_nope x = match x with Nope -> true | _ -> false
[%%expect{|
val match_nope : 'a t -> bool = <fun>
|}]

(* A GADT constructor's argument layouts must be determined even at
   instantiations where its result type says it cannot occur: [Gint]'s argument
   is never representable, so no constructor of [gadt] is usable. *)
type (_ : any) gadt =
  | Gint : abstract_any -> int gadt
  | Gany : 'a gadt
[%%expect{|
type (_ : any) gadt = Gint : abstract_any -> int gadt | Gany : 'a gadt
|}]

let bad = (Gany : string gadt)
[%%expect{|
Line 1, characters 11-15:
1 | let bad = (Gany : string gadt)
               ^^^^
Error: The representation of the constructor "Gany"
       depends on the layout of the argument of constructor "Gint",
       which this instantiation of the type string gadt does not determine.
       The layout of abstract_any is any
         because of the definition of abstract_any at line 1, characters 0-23.
       But the layout of abstract_any must be representable
         because it's the type of a constructor argument being assigned a value.
|}]

let bad = (Gany : int gadt)
[%%expect{|
Line 1, characters 11-15:
1 | let bad = (Gany : int gadt)
               ^^^^
Error: The representation of the constructor "Gany"
       depends on the layout of the argument of constructor "Gint",
       which this instantiation of the type int gadt does not determine.
       The layout of abstract_any is any
         because of the definition of abstract_any at line 1, characters 0-23.
       But the layout of abstract_any must be representable
         because it's the type of a constructor argument being assigned a value.
|}]

let bad = Gany
[%%expect{|
Line 1, characters 10-14:
1 | let bad = Gany
              ^^^^
Error: The representation of the constructor "Gany"
       depends on the layout of the argument of constructor "Gint",
       which this instantiation of the type int gadt does not determine.
       The layout of abstract_any is any
         because of the definition of abstract_any at line 1, characters 0-23.
       But the layout of abstract_any must be representable
         because it's the type of a constructor argument being assigned a value.
|}]

(* By contrast, a GADT constructor whose argument layouts are determined by its
   own result type does not poison instantiations where it cannot
   occur... *)
type (_ : any) g2 = M | D : ('a : any). 'a -> #('a * int) g2

let ok = (M : string g2)
let ok = (D #() : #(unit# * int) g2)
[%%expect{|
type (_ : any) g2 = M | D : ('a : any). 'a -> #('a * int) g2
val ok : string g2 = M
val ok : #(unit# * int) g2 = D <void>
|}]

(* ... though at an undetermined instantiation, checking [D] pins the
   instantiation to its result type. *)
let m = M
[%%expect{|
val m : #('a * int) g2 = M
|}]

(* A constraint pinning the parameter to a type of layout [any] makes the
   argument unrepresentable. *)
type ('a : any) c = K of 'a | N constraint 'a = abstract_any
[%%expect{|
type 'a c = K of 'a | N constraint 'a = abstract_any
|}]

let bad = (N : abstract_any c)
[%%expect{|
Line 1, characters 11-12:
1 | let bad = (N : abstract_any c)
               ^
Error: The representation of the constructor "N"
       depends on the layout of the argument of constructor "K",
       which this instantiation of the type abstract_any c does not determine.
       The layout of abstract_any is any
         because of the definition of abstract_any at line 1, characters 0-23.
       But the layout of abstract_any must be representable
         because it's the type of a constructor argument being assigned a value.
|}]

type (_ : any) gadt2 =
  | Ga : 'a -> 'a gadt2
  | Gint : int gadt2
[%%expect{|
type (_ : any) gadt2 = Ga : 'a -> 'a gadt2 | Gint : int gadt2
|}]

let ga x = Ga x
[%%expect{|
val ga : 'a -> 'a gadt2 = <fun>
|}]

let gint = Gint
[%%expect{|
val gint : int gadt2 = Gint
|}]

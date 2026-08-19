(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type ('a : any, 'b : any) t = Nope | Yeah of { fst : 'a; snd : 'b }
[%%expect{|
type ('a : any, 'b : any) t = Nope | Yeah of { fst : 'a; snd : 'b; }
|}]

let to_option t =
  match t with
  | Yeah { fst; snd } -> Some (fst, snd)
  | Nope -> None
[%%expect{|
val to_option : ('a, 'b) t -> ('a * 'b) option = <fun>
|}]

let is_yeah (type a : any) (type b : any) (t : (a, b) t) =
  match t with Yeah _ -> true | Nope -> false
[%%expect{|
val is_yeah : ('a : any) ('b : any). ('a, 'b) t -> bool = <fun>
|}]

let to_option (t : (int, string) t) =
  match t with
  | Yeah { fst; snd } -> Some (fst, snd)
  | Nope -> None
[%%expect{|
val to_option : (int, string) t -> (int * string) option = <fun>
|}]

let to_option (type a : value) (type b : value) (t : (a, b) t) =
  match t with
  | Yeah { fst; snd } -> Some (fst, snd)
  | Nope -> None
[%%expect{|
val to_option : ('a, 'b) t -> ('a * 'b) option = <fun>
|}]

let is_yeah (t : (int64#, 'b) t) = match t with Yeah _ -> true | Nope -> false
[%%expect{|
val is_yeah : ('b : any). (int64#, 'b) t -> bool = <fun>
|}]

let is_yeah (type a : bits64) (type b : value) (t : (a, b) t) =
  match t with Yeah _ -> true | Nope -> false
[%%expect{|
val is_yeah : ('a : bits64) 'b. ('a, 'b) t -> bool = <fun>
|}]

let of_option o =
  match o with Some (fst, snd) -> Yeah { fst; snd } | None -> Nope
[%%expect{|
val of_option : ('a * 'b) option -> ('a, 'b) t = <fun>
|}]

let nope = Nope
[%%expect{|
val nope : ('a : any) ('b : any). ('a, 'b) t = Nope
|}]

let nope : 'a 'b. 'a t = Nope
[%%expect{|
Line 1, characters 18-22:
1 | let nope : 'a 'b. 'a t = Nope
                      ^^^^
Error: The type constructor "t" expects 2 argument(s),
       but is here applied to 1 argument(s)
|}]

let yeah fst snd = Yeah { fst; snd }
[%%expect{|
val yeah : 'a -> 'b -> ('a, 'b) t = <fun>
|}]

let yeah (fst : int) (snd : string) = Yeah { fst; snd }
[%%expect{|
val yeah : int -> string -> (int, string) t = <fun>
|}]

let yeah fst snd : (int, string) t = Yeah { fst; snd }
[%%expect{|
val yeah : int -> string -> (int, string) t = <fun>
|}]

let yeah (type a : value) (type b : value) (fst : a) (snd : b) =
  Yeah { fst; snd }
[%%expect{|
val yeah : 'a -> 'b -> ('a, 'b) t = <fun>
|}]

let yeah (type a : value) (type b : value) fst snd : (a, b) t =
  Yeah { fst; snd }
[%%expect{|
val yeah : 'a -> 'b -> ('a, 'b) t = <fun>
|}]

let yeah (fst : int64#) (snd : float32#) = Yeah { fst; snd }
[%%expect{|
val yeah : int64# -> float32# -> (int64#, float32#) t = <fun>
|}]

let yeah (type a : bits64) (type b : float32) (fst : a) (snd : b) =
  Yeah { fst; snd }
[%%expect{|
val yeah : ('a : bits64) ('b : float32). 'a -> 'b -> ('a, 'b) t = <fun>
|}]

let yeah (type a : bits64) (type b : float32) fst snd : (a, b) t =
  Yeah { fst; snd }
[%%expect{|
val yeah : ('a : bits64) ('b : float32). 'a -> 'b -> ('a, 'b) t = <fun>
|}]

(* Test that typing and genprintval work when the actual type has kind value *)
let test_block_with_value = Yeah { fst = 1; snd = 2 }
[%%expect {|
val test_block_with_value : (int, int) t = Yeah {fst = 1; snd = 2}
|}]

let test_block = Yeah { fst = #1L; snd = #2L }
[%%expect {|
val test_block : (int64#, int64#) t = Yeah {fst = <abstr>; snd = <abstr>}
|}]

type ('a : any) any_list = Nil | Cons of { head : 'a; tail : 'a any_list }

(* CR-someday lmaurer: This should work without the kind ascriptions but it
   seems we don't infer [value] as we normally would *)
let rec map_unboxed_pair
          (l : #(('a : value) * ('b : value)) any_list)
          ~(f : (#('a * 'b) -> #('c * 'd))) =
  match l with
  | Nil -> Nil
  | Cons { head; tail } ->
    Cons { head = f head; tail = map_unboxed_pair tail ~f }

let rec box_all (l : #('a * 'b) any_list) : ('a * 'b) any_list =
  match l with
  | Nil -> Nil
  | Cons { head = #(a, b); tail } -> Cons { head = (a, b); tail = box_all tail }

let test =
  Cons { head = #(1, 2);
         tail = Cons { head = #(3, 4);
                       tail = Cons { head = #(5, 6); tail = Nil } } }
  |> map_unboxed_pair ~f:(fun #(a, b) -> #(a + 1, b + 2))
  |> box_all

[%%expect {|
type ('a : any) any_list = Nil | Cons of { head : 'a; tail : 'a any_list; }
val map_unboxed_pair :
  #('a * 'b) any_list -> f:(#('a * 'b) -> #('c * 'd)) -> #('c * 'd) any_list =
  <fun>
val box_all : #('a * 'b) any_list -> ('a * 'b) any_list = <fun>
val test : (int * int) any_list =
  Cons
   {head = (2, 4);
    tail = Cons {head = (4, 6); tail = Cons {head = (6, 8); tail = Nil}}}
|}]

(* Test that an inline record is built with its constructor's tag rather than
   tag 0, when other non-constant constructors precede it *)
type ('a : any) tagged = Ia of 'a | Ib of { x : 'a; y : int }
[%%expect{|
type ('a : any) tagged = Ia of 'a | Ib of { x : 'a; y : int; }
|}]

let v = Ib { x = 5; y = 7 }
[%%expect{|
val v : int tagged = Ib {x = 5; y = 7}
|}]

let classify (t : int tagged) =
  match t with
  | Ia _ -> "Ia"
  | Ib { x; y } -> Printf.sprintf "Ib x=%d y=%d" x y
[%%expect{|
val classify : int tagged -> string = <fun>
|}]

let test = classify (Ib { x = 5; y = 7 })
[%%expect{|
val test : string = "Ib x=5 y=7"
|}]

let test = classify (Ia 3)
[%%expect{|
val test : string = "Ia"
|}]

(* ... including when the inline record is mixed *)
let v = Ib { x = #2.5; y = 7 }
[%%expect{|
val v : float# tagged = Ib {x = <abstr>; y = 7}
|}]

let test (t : float# tagged) =
  match t with
  | Ia _ -> "Ia"
  | Ib { y; _ } -> Printf.sprintf "Ib y=%d" y
[%%expect{|
val test : float# tagged -> string = <fun>
|}]

let test = test (Ib { x = #2.5; y = 7 })
[%%expect{|
val test : string = "Ib y=7"
|}]

(* Field projection and mutation on inline records with [any] *)
type ('a : any) mut = Skip of 'a | Mut of { mutable m : 'a; k : int }
[%%expect{|
type ('a : any) mut = Skip of 'a | Mut of { mutable m : 'a; k : int; }
|}]

let test =
  let v : int mut = Mut { m = 1; k = 2 } in
  (match v with Mut r -> r.m <- r.m + 10 | Skip _ -> ());
  v
[%%expect{|
val test : int mut = Mut {m = 11; k = 2}
|}]

(* Functional update of an inline record with [any] *)
let test (t : int tagged) =
  match t with
  | Ia _ -> t
  | Ib r -> Ib { r with x = r.x + 100 }
[%%expect{|
val test : int tagged -> int tagged = <fun>
|}]

let test = test (Ib { x = 5; y = 7 })
[%%expect{|
val test : int tagged = Ib {x = 105; y = 7}
|}]

(* An [@@unboxed] inline record with an [any] field is still represented by
   its field *)
type ('a : any) ubx = U of { u : 'a } [@@unboxed]
[%%expect{|
type ('a : any) ubx = U of { u : 'a; } [@@unboxed]
|}]

let test = U { u = 42 }
[%%expect{|
val test : int ubx = <unknown constructor>
|}]

let test = match U { u = 42 } with U { u } -> u
[%%expect{|
val test : int = 42
|}]

(* Existential [any] fields: construction grounds the existential, but
   projection is rejected *)
type ebox = E : ('a : any). { v : 'a; k : int } -> ebox
[%%expect{|
type ebox = E : ('a : any). { v : 'a; k : int; } -> ebox
|}]

let b = E { v = 5; k = 1 }
[%%expect{|
val b : ebox = E {v = <poly>; k = 1}
|}]

let k = match b with E { k; _ } -> k
[%%expect{|
Line 1, characters 23-31:
1 | let k = match b with E { k; _ } -> k
                           ^^^^^^^^
Error: Record element types must have a representable layout.
       The layout of $a is any
         because of the definition of ebox at line 1, characters 0-55.
       But the layout of $a must be representable
         because it's the type of a field in a record being projected from.
|}]

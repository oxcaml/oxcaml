(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type ('a : any, 'b : any) t = Nope | Yeah of { fst : 'a; snd : 'b }
[%%expect{|
type ('a : any, 'b : any) t = Nope | Yeah of { fst : 'a; snd : 'b; }
|}]

(* CR-someday lmaurer: Would be good to infer [value] as the kind here as we do
   for the record and tuple-constructor cases. *)
let to_option t =
  match t with
  | Yeah { fst; snd } -> Some (fst, snd)
  | Nope -> None
[%%expect{|
Line 3, characters 9-21:
3 |   | Yeah { fst; snd } -> Some (fst, snd)
             ^^^^^^^^^^^^
Error: Cannot access record with unrepresentable field.
       The record has type ('a, 'b) t.Yeah,
       whose field fst is not representable.
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

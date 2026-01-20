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
val nope : ('a : any). 'a t = Nope
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

let rec to_list (l : _ any_list) : _ list =
  match l with
  | [] -> []
  | a :: l' -> a :: to_list l'

let test =
  [#(1, 2); #(3, 4); #(5, 6)]
  |> map_unboxed_pair ~f:(fun #(a, b) -> #(a + 1, b + 2))
  |> box_all
  |> to_list

[%%expect {|
type ('a : any) any_list = [] | (::) of 'a * 'a any_list
val map_unboxed_pair :
  #('a * 'b) any_list -> f:(#('a * 'b) -> #('c * 'd)) -> #('c * 'd) any_list =
  <fun>
val box_all : #('a * 'b) any_list -> ('a * 'b) any_list = <fun>
val to_list : 'a any_list -> 'a list = <fun>
val test : (int * int) list = [(2, 4); (4, 6); (6, 8)]
|}]

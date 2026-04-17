(* TEST
 include stdlib_stable;
 expect;
*)

module Iarray = Stdlib_stable.IarrayLabels;;

external ( .:() ) : 'a iarray -> int -> 'a = "%array_safe_get";;

(** Create some immutable and mutable arrays *)

let iarray  : int   iarray = [|1;2;3;4;5|];;
let iarray_local () = exclave_ Iarray.init_local 5 (fun x -> x + 1);;
let ifarray : float iarray = [:1.5;2.5;3.5;4.5;5.5:];;
let ifarray_local () =
  exclave_ Iarray.init_local 5 ~f:(fun x -> Int.to_float x +. 1.5);;

let marray  : int   array = [|1;2;3;4;5|];;
let mfarray : float array = [|1.5;2.5;3.5;4.5;5.5|];;

external globalize_float : local_ float -> float = "%obj_dup";;
external globalize_string : local_ string -> string = "%obj_dup";;
let globalize_int_iarray (local_ ia) =
  Iarray.map_local_input ~f:(fun x : int -> x) ia;;

let rec list_map_local_input (local_ f) (local_ list) =
  match list with
  | [] -> []
  | x :: xs -> f x :: list_map_local_input f xs;;

[%%expect{|
module Iarray = Stdlib_stable.IarrayLabels
external ( .:() ) : 'a iarray -> int -> 'a = "%array_safe_get"
val iarray : int iarray = [:1; 2; 3; 4; 5:]
Line 8, characters 31-48:
8 | let iarray_local () = exclave_ Iarray.init_local 5 (fun x -> x + 1);;
                                   ^^^^^^^^^^^^^^^^^
Warning 6 [labels-omitted]: label "f" was omitted in the application of this
  function.

val iarray_local : unit -> int iarray @ local = <fun>
val ifarray : float iarray = [:1.5; 2.5; 3.5; 4.5; 5.5:]
val ifarray_local : unit -> float iarray @ local = <fun>
val marray : int array = [|1; 2; 3; 4; 5|]
val mfarray : float array = [|1.5; 2.5; 3.5; 4.5; 5.5|]
external globalize_float : float @ local -> float = "%obj_dup"
external globalize_string : string @ local -> string = "%obj_dup"
val globalize_int_iarray : int iarray @ local -> int iarray = <fun>
val list_map_local_input :
  ('a @ local -> 'b) @ local -> 'a list @ local -> 'b list = <fun>
|}];;

(** Pattern-match on some immutable arrays, and check the typing of array
    patterns, both mutable and immutable *)

match iarray with
| [::]          -> "empty"
| [:1;2;3;4;5:] -> "1--5"
| _             -> "who knows?"
;;
[%%expect{|
- : string = "1--5"
|}];;

match ifarray with
| [::]                    -> "empty"
| [:1.5;2.5;3.5;4.5;5.5:] -> "1.5--5.5"
| _                       -> "who knows?"
;;
[%%expect{|
- : string = "1.5--5.5"
|}];;

match iarray with
| [::]          -> "empty"
| [:1;2;3;4;6:] -> "1--5"
| _             -> "who knows?"
;;
[%%expect{|
- : string = "who knows?"
|}];;

match ifarray with
| [::]          -> "empty"
| [:1;2;3;4;5:] -> "1--5"
| _             -> "who knows?"
;;
[%%expect{|
Line 3, characters 4-5:
3 | | [:1;2;3;4;5:] -> "1--5"
        ^
Error: This pattern matches values of type "int"
       but a pattern was expected which matches values of type "float"
Hint: Did you mean "1."?
|}];;

match marray with
| [::]          -> "empty"
| [:1;2;3;4;5:] -> "1--5"
| _             -> "who knows?"
;;
[%%expect{|
Line 2, characters 2-6:
2 | | [::]          -> "empty"
      ^^^^
Error: This pattern matches values of type "'a iarray"
       but a pattern was expected which matches values of type "int array"
|}];;

match iarray with
| [||]          -> "empty"
| [|1;2;3;4;5|] -> "1--5"
| _             -> "who knows?"
;;
[%%expect{|
Line 2, characters 2-6:
2 | | [||]          -> "empty"
      ^^^^
Error: This pattern matches values of type "'a array"
       but a pattern was expected which matches values of type "int iarray"
|}];;

(** Confirm that immutable and mutable arrays have the same representation, even
    when they're different objects *)

Obj.repr iarray = Obj.repr marray;;
[%%expect{|
- : bool = true
|}];;

Obj.repr ifarray = Obj.repr mfarray;;
[%%expect{|
- : bool = true
|}];;

iarray == Obj.magic marray;;
[%%expect{|
- : bool = false
|}];;

ifarray == Obj.magic mfarray;;
[%%expect{|
- : bool = false
|}];;

(** Confirm that immutable and mutable arrays don't collide *)

Obj.repr iarray <> Obj.repr ifarray;;
[%%expect{|
- : bool = true
|}];;

Obj.repr marray <> Obj.repr mfarray;;
[%%expect{|
- : bool = true
|}];;

(** Test basic functionality: One or a few tests for every function in [Iarray].
    We test both success and error cases, and in general try to have coverage of
    edge cases.  Comments are attached everywhere something subtle is being
    checked. *)

Iarray.length iarray, Iarray.length ifarray;;
[%%expect{|
- : int * int = (5, 5)
|}];;

iarray.:(0), Iarray.get iarray 1, ifarray.:(2), Iarray.get ifarray 3;;
[%%expect{|
- : int * int * float * float = (1, 2, 3.5, 4.5)
|}];;

iarray.:(10)
[%%expect{|
Exception: Invalid_argument "index out of bounds".
|}];;

Iarray.get iarray (-1);;
[%%expect{|
Exception: Invalid_argument "index out of bounds".
|}];;

ifarray.:(-10);;
[%%expect{|
Exception: Invalid_argument "index out of bounds".
|}];;

Iarray.get ifarray 5;;
[%%expect{|
Exception: Invalid_argument "index out of bounds".
|}];;

Iarray.init 10 ~f:(fun x -> x * 2);;
[%%expect{|
- : int iarray = [:0; 2; 4; 6; 8; 10; 12; 14; 16; 18:]
|}];;

globalize_int_iarray (Iarray.init_local 10 ~f:(fun x -> x * 2));;
[%%expect{|
- : int iarray = [:0; 2; 4; 6; 8; 10; 12; 14; 16; 18:]
|}];;

Iarray.append iarray iarray;;
[%%expect{|
- : int iarray = [:1; 2; 3; 4; 5; 1; 2; 3; 4; 5:]
|}];;

globalize_int_iarray (Iarray.append_local
    (Iarray.init_local 5 ~f:(fun x -> x)) (iarray_local ()));;
[%%expect{|
- : int iarray = [:0; 1; 2; 3; 4; 1; 2; 3; 4; 5:]
|}];;

Iarray.concat [];;
[%%expect{|
- : 'a iarray = [::]
|}];;

Iarray.concat [ Iarray.init 1 ~f:(fun x ->   1 + x)
              ; Iarray.init 2 ~f:(fun x ->  20 + x)
              ; Iarray.init 3 ~f:(fun x -> 300 + x) ];;
[%%expect{|
- : int iarray = [:1; 20; 21; 300; 301; 302:]
|}];;

globalize_int_iarray
  (Iarray.concat_local [ Iarray.init_local 1 ~f:(fun x ->   1 + x)
                       ; Iarray.init_local 2 ~f:(fun x ->  20 + x)
                       ; Iarray.init_local 3 ~f:(fun x -> 300 + x) ]);;
[%%expect{|
- : int iarray = [:1; 20; 21; 300; 301; 302:]
|}];;

Iarray.sub iarray ~pos:0 ~len:2, Iarray.sub iarray ~pos:2 ~len:3;;
[%%expect{|
- : int iarray * int iarray = ([:1; 2:], [:3; 4; 5:])
|}];;

Iarray.sub iarray ~pos:(-1) ~len:3;;
[%%expect{|
Exception: Invalid_argument "Iarray.sub".
|}];;

Iarray.sub iarray ~pos:1 ~len:(-3);;
[%%expect{|
Exception: Invalid_argument "Iarray.sub".
|}];;

Iarray.sub iarray ~pos:3 ~len:10;;
[%%expect{|
Exception: Invalid_argument "Iarray.sub".
|}];;

let iarray = iarray_local () in
globalize_int_iarray (Iarray.sub_local iarray ~pos:0 ~len:2),
globalize_int_iarray (Iarray.sub_local iarray ~pos:2 ~len:3);;
[%%expect{|
- : int iarray * int iarray = ([:1; 2:], [:3; 4; 5:])
|}];;

let iarray = iarray_local () in
globalize_int_iarray (Iarray.sub_local iarray ~pos:(-1) ~len:3);;
[%%expect{|
Exception: Invalid_argument "Iarray.sub".
|}];;

let iarray = iarray_local () in
globalize_int_iarray (Iarray.sub_local iarray ~pos:1 ~len:(-3));;
[%%expect{|
Exception: Invalid_argument "Iarray.sub".
|}];;

let iarray = iarray_local () in
globalize_int_iarray (Iarray.sub_local iarray ~pos:3 ~len:10);;
[%%expect{|
Exception: Invalid_argument "Iarray.sub".
|}];;

Iarray.to_list iarray;;
[%%expect{|
- : int list = [1; 2; 3; 4; 5]
|}];;

list_map_local_input (fun x : int -> x)
  (Iarray.to_list_local (iarray_local ()));;
[%%expect{|
- : int list = [1; 2; 3; 4; 5]
|}];;

Iarray.of_list [10;20;30];;
[%%expect{|
- : int iarray = [:10; 20; 30:]
|}];;

globalize_int_iarray (Iarray.of_list_local [10;20;30]);;
[%%expect{|
- : int iarray = [:10; 20; 30:]
|}];;

Iarray.to_array iarray;;
[%%expect{|
- : int array = [|1; 2; 3; 4; 5|]
|}];;

Iarray.of_array mfarray;;
[%%expect{|
- : float iarray = [:1.5; 2.5; 3.5; 4.5; 5.5:]
|}];;

(* [Iarray.to_array] creates a fresh mutable array every time *)
Iarray.to_array iarray == marray;;
[%%expect{|
- : bool = false
|}];;

(* [Iarray.to_array] creates a fresh mutable array every time *)
Iarray.to_array ifarray == Iarray.to_array ifarray;;
[%%expect{|
- : bool = false
|}];;

(* Round-tripping from and to an [iarray] creates a fresh copy every time *)
Iarray.of_array (Iarray.to_array iarray) == iarray;;
[%%expect{|
- : bool = false
|}];;

let sum = ref 0. in
Iarray.iter ~f:(fun x -> sum := !sum +. x) ifarray;
!sum;;
[%%expect{|
- : float = 17.5
|}];;

let open struct
  external (+.) :
    (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt]) =
    "%addfloat"
end in
let sum = ref 0. in
Iarray.iter_local ~f:(fun x -> sum := globalize_float (!sum +. x))
  [:1.5;2.5;3.5;4.5;5.5:];
!sum;;
[%%expect{|
- : float = 17.5
|}];;

let total = ref 0 in
Iarray.iteri ~f:(fun i x -> total := !total + i*x) iarray;
!total;;
[%%expect{|
- : int = 40
|}];;

let total = ref 0 in
Iarray.iteri_local ~f:(fun i x -> total := !total + i*x) [:1;2;3;4;5:];
!total;;
[%%expect{|
- : int = 40
|}];;

Iarray.map ~f:Int.neg iarray;;
[%%expect{|
- : int iarray = [:-1; -2; -3; -4; -5:]
|}];;

globalize_int_iarray (Iarray.map_local ~f:Int.neg
                        (iarray_local ()));;
[%%expect{|
- : int iarray = [:-1; -2; -3; -4; -5:]
|}];;

Iarray.map_local_input ~f:Int.neg (iarray_local ());;
[%%expect{|
- : int iarray = [:-1; -2; -3; -4; -5:]
|}];;

globalize_int_iarray (Iarray.map_local_output ~f:Int.neg iarray);;
[%%expect{|
- : int iarray = [:-1; -2; -3; -4; -5:]
|}];;

Iarray.mapi ~f:(fun i x -> i, 10.*.x) ifarray;;
[%%expect{|
- : (int * float) iarray =
[:(0, 15.); (1, 25.); (2, 35.); (3, 45.); (4, 55.):]
|}];;

let globalize_int_float_iarray (local_ ia) =
  Iarray.map_local_input ~f:(fun ((i : int), f) -> i, globalize_float f) ia;;
[%%expect{|
val globalize_int_float_iarray :
  (int * float) iarray @ local -> (int * float) iarray = <fun>
|}];;

globalize_int_float_iarray
  (Iarray.mapi_local ~f:(fun i x -> exclave_ i, 10.*.x)
     (ifarray_local ()));;
[%%expect{|
- : (int * float) iarray =
[:(0, 15.); (1, 25.); (2, 35.); (3, 45.); (4, 55.):]
|}];;

Iarray.mapi_local_input ~f:(fun i x -> i, 10. *. globalize_float x)
  (ifarray_local ());;
[%%expect{|
- : (int * float) iarray =
[:(0, 15.); (1, 25.); (2, 35.); (3, 45.); (4, 55.):]
|}];;

globalize_int_float_iarray
  (Iarray.mapi_local_output ~f:(fun i x -> exclave_ i, 10.*.x) ifarray);;
[%%expect{|
- : (int * float) iarray =
[:(0, 15.); (1, 25.); (2, 35.); (3, 45.); (4, 55.):]
|}];;

Iarray.fold_left ~f:(fun acc x -> -x :: acc) ~init:[] iarray;;
[%%expect{|
- : int list = [-5; -4; -3; -2; -1]
|}];;

list_map_local_input (fun x : int -> x)
  (Iarray.fold_left_local ~f:(fun acc x -> exclave_ -x :: acc) ~init:[]
     (iarray_local ()));;
[%%expect{|
- : int list = [-5; -4; -3; -2; -1]
|}];;

Iarray.fold_left_local_input ~f:(fun acc x -> -x :: acc) ~init:[]
  (iarray_local ());;
[%%expect{|
- : int list = [-5; -4; -3; -2; -1]
|}];;

list_map_local_input (fun x : int -> x)
  (Iarray.fold_left_local_output ~f:(fun acc x -> exclave_ -x :: acc) ~init:[]
     iarray);;
[%%expect{|
- : int list = [-5; -4; -3; -2; -1]
|}];;

Iarray.fold_left_map ~f:(fun acc x -> acc + x, string_of_int x) ~init:0 iarray;;
[%%expect{|
- : int * string iarray = (15, [:"1"; "2"; "3"; "4"; "5":])
|}];;

let n, strs =
  Iarray.fold_left_map_local ~f:(fun acc x -> acc + x, string_of_int x) ~init:0 iarray
in
n, Iarray.map_local_input ~f:globalize_string strs;;
[%%expect{|
- : int * string iarray = (5, [:"1"; "2"; "3"; "4"; "5":])
|}];;

Iarray.fold_left_map_local_input ~f:(fun acc x -> acc + x, string_of_int x) ~init:0 iarray;;
[%%expect{|
- : int * string iarray = (15, [:"1"; "2"; "3"; "4"; "5":])
|}];;

let n, strs =
  Iarray.fold_left_map_local_output
     ~f:(fun acc x -> acc + x, string_of_int x) ~init:0 iarray
in
n, Iarray.map_local_input ~f:globalize_string strs;;
[%%expect{|
- : int * string iarray = (5, [:"1"; "2"; "3"; "4"; "5":])
|}];;

(* Confirm the function isn't called on the empty immutable array *)
Iarray.fold_left_map ~f:(fun _ _ -> assert false) ~init:0 [::];;
[%%expect{|
- : int * 'a iarray = (0, [::])
|}];;

Iarray.fold_right ~f:(fun x acc -> -.x :: acc) ifarray ~init:[];;
[%%expect{|
- : float list = [-1.5; -2.5; -3.5; -4.5; -5.5]
|}];;

list_map_local_input globalize_float
  (Iarray.fold_right_local ~f:(fun x acc -> exclave_ -.x :: acc)
     (ifarray_local ()) ~init:[]);;
[%%expect{|
- : float list = [-1.5; -2.5; -3.5; -4.5; -5.5]
|}];;

Iarray.fold_right_local_input ~f:(fun x acc -> -. (globalize_float x) :: acc)
  (ifarray_local ()) ~init:[];;
[%%expect{|
- : float list = [-1.5; -2.5; -3.5; -4.5; -5.5]
|}];;

list_map_local_input globalize_float
  (Iarray.fold_right_local_output ~f:(fun x acc -> exclave_ -.x :: acc)
     ifarray ~init:[]);;
[%%expect{|
- : float list = [-1.5; -2.5; -3.5; -4.5; -5.5]
|}];;

let ints   = ref 0  in
let floats = ref 0. in
Iarray.iter2
  ~f:(fun i f ->
    ints   := i +  !ints;
    floats := f +. !floats)
  iarray
  ifarray;
!ints, !floats;;
[%%expect{|
- : int * float = (15, 17.5)
|}];;

let ints   = ref 0  in
let floats = ref 0. in
Iarray.iter2_local
  ~f:(fun i f ->
     ints   := i +  !ints;
     floats := globalize_float (f +. !floats))
  (iarray_local ()) (ifarray_local ());
!ints, !floats;;
[%%expect{|
- : int * float = (15, 17.5)
|}];;

let ints   = ref 0  in
let floats = ref 0. in
Iarray.iter2_local_first
  ~f:(fun i f ->
     ints   := i +  !ints;
     floats := f +. !floats)
  (iarray_local ()) ifarray;
!ints, !floats;;
[%%expect{|
- : int * float = (15, 17.5)
|}];;

let ints   = ref 0  in
let floats = ref 0. in
Iarray.iter2_local_second
  ~f:(fun i f ->
     ints   := i +  !ints;
     floats := globalize_float (f +. !floats))
  iarray (ifarray_local ());
!ints, !floats;;
[%%expect{|
- : int * float = (15, 17.5)
|}];;

Iarray.map2 ~f:(fun i f -> f, i) iarray ifarray;;
[%%expect{|
- : (float * int) iarray =
[:(1.5, 1); (2.5, 2); (3.5, 3); (4.5, 4); (5.5, 5):]
|}];;

Iarray.map_local_input ~f:(fun (f, (i : int)) -> globalize_float f, i)
  (Iarray.map2_local ~f:(fun i f -> exclave_ f, i)
     (iarray_local ()) (ifarray_local ()));;
[%%expect{|
- : (float * int) iarray =
[:(1.5, 1); (2.5, 2); (3.5, 3); (4.5, 4); (5.5, 5):]
|}];;

Iarray.map2_local_inputs ~f:(fun (i : int) f -> globalize_float f, i)
  (iarray_local ()) (ifarray_local ());;
[%%expect{|
- : (float * int) iarray =
[:(1.5, 1); (2.5, 2); (3.5, 3); (4.5, 4); (5.5, 5):]
|}];;

Iarray.map_local_input ~f:(fun (f, (i : int)) -> globalize_float f, i)
  (Iarray.map2_local_output ~f:(fun i f -> exclave_ f, i)
     iarray ifarray);;
[%%expect{|
- : (float * int) iarray =
[:(1.5, 1); (2.5, 2); (3.5, 3); (4.5, 4); (5.5, 5):]
|}];;

Iarray.map2_local_first_input ~f:(fun (i : int) f -> f, i)
  (iarray_local ()) ifarray;;
[%%expect{|
- : (float * int) iarray =
[:(1.5, 1); (2.5, 2); (3.5, 3); (4.5, 4); (5.5, 5):]
|}];;

Iarray.map2_local_second_input ~f:(fun i f -> globalize_float f, i)
  iarray (ifarray_local ());;
[%%expect{|
- : (float * int) iarray =
[:(1.5, 1); (2.5, 2); (3.5, 3); (4.5, 4); (5.5, 5):]
|}];;

Iarray.map_local_input ~f:(fun (f, (i : int)) -> globalize_float f, i)
  (Iarray.map2_local_first_input_and_output
     ~f:(fun i f -> exclave_ f, i)
     (iarray_local ()) ifarray);;
[%%expect{|
- : (float * int) iarray =
[:(1.5, 1); (2.5, 2); (3.5, 3); (4.5, 4); (5.5, 5):]
|}];;

Iarray.map_local_input ~f:(fun (f, (i : int)) -> globalize_float f, i)
  (Iarray.map2_local_second_input_and_output
     ~f:(fun i f -> exclave_ f, i)
     iarray (ifarray_local ()));;
[%%expect{|
- : (float * int) iarray =
[:(1.5, 1); (2.5, 2); (3.5, 3); (4.5, 4); (5.5, 5):]
|}];;

Iarray.for_all ~f:(fun i -> i > 0) iarray;;
[%%expect{|
- : bool = true
|}];;

Iarray.for_all ~f:(fun f -> f < 5.) ifarray;;
[%%expect{|
- : bool = false
|}];;

Iarray.for_all_local ~f:(fun i -> i > 0) (iarray_local ());;
[%%expect{|
- : bool = true
|}];;

Iarray.for_all_local ~f:(fun f -> f < 5.) (ifarray_local ());;
[%%expect{|
- : bool = false
|}];;

Iarray.exists ~f:(fun f -> f < 5.) ifarray;;
[%%expect{|
- : bool = true
|}];;

Iarray.exists ~f:(fun i -> i > 10) iarray;;
[%%expect{|
- : bool = false
|}];;

Iarray.exists_local ~f:(fun f -> f < 5.) (ifarray_local ());;
[%%expect{|
- : bool = true
|}];;

Iarray.exists_local ~f:(fun i -> i > 10) (iarray_local ());;
[%%expect{|
- : bool = false
|}];;

Iarray.for_all2 ~f:(fun i f -> Float.of_int i < f) iarray ifarray;;
[%%expect{|
- : bool = true
|}];;

Iarray.for_all2 ~f:(fun f i -> i = 1 && f = 1.5) ifarray iarray;;
[%%expect{|
- : bool = false
|}];;

Iarray.for_all2_local ~f:(fun i f -> Float.of_int i < f)
  (iarray_local ()) (ifarray_local ());;
[%%expect{|
- : bool = true
|}];;

Iarray.for_all2_local ~f:(fun f i -> i = 1 && f = 1.5)
  (ifarray_local ()) (iarray_local ());;
[%%expect{|
- : bool = false
|}];;

Iarray.for_all2_local_first ~f:(fun i f -> Float.of_int i < f)
  (iarray_local ()) ifarray;;
[%%expect{|
- : bool = true
|}];;

Iarray.for_all2_local_first ~f:(fun f i -> i = 1 && f = 1.5)
  (ifarray_local ()) iarray;;
[%%expect{|
- : bool = false
|}];;

Iarray.for_all2_local_second ~f:(fun i f -> Float.of_int i < f)
  iarray (ifarray_local ());;
[%%expect{|
- : bool = true
|}];;

Iarray.for_all2_local_second ~f:(fun f i -> i = 1 && f = 1.5)
  ifarray (iarray_local ());;
[%%expect{|
- : bool = false
|}];;

Iarray.exists2 ~f:(fun f i -> Float.of_int i +. f = 8.5) ifarray iarray;;
[%%expect{|
- : bool = true
|}];;

Iarray.exists2 ~f:(fun i f -> Float.of_int i > f) iarray ifarray;;
[%%expect{|
- : bool = false
|}];;

Iarray.exists2_local ~f:(fun f i -> Float.of_int i +. f = 8.5)
  (ifarray_local ()) (iarray_local ());;
[%%expect{|
- : bool = true
|}];;

Iarray.exists2_local ~f:(fun i f -> Float.of_int i > f)
  (iarray_local ()) (ifarray_local ());;
[%%expect{|
- : bool = false
|}];;

Iarray.exists2_local_first ~f:(fun f i -> Float.of_int i +. f = 8.5)
  (ifarray_local ()) iarray;;
[%%expect{|
- : bool = true
|}];;

Iarray.exists2_local_first ~f:(fun i f -> Float.of_int i > f)
  (iarray_local ()) ifarray;;
[%%expect{|
- : bool = false
|}];;
Iarray.exists2_local_second ~f:(fun f i -> Float.of_int i +. f = 8.5)
  ifarray (iarray_local ());;
[%%expect{|
- : bool = true
|}];;

Iarray.exists2_local_second ~f:(fun i f -> Float.of_int i > f)
  iarray (ifarray_local ());;
[%%expect{|
- : bool = false
|}];;

Iarray.mem 3 ~set:iarray, Iarray.mem 3.5 ~set:ifarray;;
[%%expect{|
- : bool * bool = (true, true)
|}];;

Iarray.mem 30 ~set:iarray, Iarray.mem 35. ~set:ifarray;;
[%%expect{|
- : bool * bool = (false, false)
|}];;

let x = ref 0 in
Iarray.memq x ~set:(Iarray.init 3 ~f:(Fun.const x));;
[%%expect{|
- : bool = true
|}];;

Iarray.memq (ref 0) ~set:(Iarray.init 3 ~f:(Fun.const (ref 0)))
[%%expect{|
- : bool = false
|}];;

Iarray.find_opt ~f:(fun x -> x*x  > 5)  iarray,
Iarray.find_opt ~f:(fun x -> x*.x > 5.) ifarray;;
[%%expect{|
- : int option * float option = (Some 3, Some 2.5)
|}];;

Iarray.find_opt ~f:(fun x -> x*x  > 50)  iarray,
Iarray.find_opt ~f:(fun x -> x*.x > 50.) ifarray;;
[%%expect{|
- : int option * float option = (None, None)
|}];;

let globalize_int_option (local_ opt) = match opt with
  | None -> None
  | Some (x : int) -> Some x

let globalize_float_option (local_ opt) = match opt with
  | None -> None
  | Some x -> Some (globalize_float x)

[%%expect{|
val globalize_int_option : int option @ local -> int option = <fun>
val globalize_float_option : float option @ local -> float option = <fun>
|}];;

globalize_int_option (Iarray.find_opt_local ~f:(fun x -> x*x  > 5)
                        (iarray_local ())),
globalize_float_option (Iarray.find_opt_local ~f:(fun x -> x*.x > 5.)
                          (ifarray_local ()));;
[%%expect{|
- : int option * float option = (Some 3, Some 2.5)
|}];;

globalize_int_option (Iarray.find_opt_local ~f:(fun x -> x*x  > 50)
                        (iarray_local ())),
globalize_float_option (Iarray.find_opt_local ~f:(fun x -> x*.x > 50.)
                          (ifarray_local ()));;
[%%expect{|
- : int option * float option = (None, None)
|}];;

Iarray.find_map ~f:(fun x -> if x mod 2 = 0
                          then Some (x / 2)
                          else None)
                iarray,
Iarray.find_map ~f:(fun x -> if Float.rem x 2. = 0.5
                          then Some ((x -. 0.5) /. 2.)
                          else None)
                ifarray;;
[%%expect{|
- : int option * float option = (Some 1, Some 1.)
|}];;

Iarray.find_map ~f:(fun x -> if x mod 7 = 0
                          then Some (x / 7)
                          else None)
                iarray,
Iarray.find_map ~f:(fun x -> if Float.rem x 7. = 0.5
                          then Some ((x -. 0.5) /. 7.)
                          else None)
                ifarray;;
[%%expect{|
- : int option * float option = (None, None)
|}];;

external rem :
  (float[@local_opt]) -> (float[@local_opt]) -> float =
  "caml_fmod_float" "fmod" [@@unboxed] [@@noalloc];;
[%%expect{|
external rem : (float [@local_opt]) -> (float [@local_opt]) -> float
  = "caml_fmod_float" "fmod" [@@unboxed] [@@noalloc]
|}];;

globalize_int_option
  (Iarray.find_map_local ~f:(fun x -> if x mod 2 = 0
                          then Some (x / 2)
                          else None)
                (iarray_local ())),
globalize_float_option
  (Iarray.find_map_local ~f:(fun x -> if rem x 2. = 0.5
                          then exclave_ Some ((x -. 0.5) /. 2.)
                          else None)
                (ifarray_local ()));;
[%%expect{|
- : int option * float option = (Some 1, Some 1.)
|}];;

globalize_int_option
  (Iarray.find_map_local ~f:(fun x -> if x mod 7 = 0
                          then Some (x / 7)
                          else None)
                (iarray_local ())),
globalize_float_option
  (Iarray.find_map_local ~f:(fun x -> if rem x 7. = 0.5
                          then exclave_ Some ((x -. 0.5) /. 7.)
                          else None)
                (ifarray_local ()));;
[%%expect{|
- : int option * float option = (None, None)
|}];;

Iarray.find_map_local_input ~f:(fun x -> if x mod 2 = 0
                          then Some (x / 2)
                          else None)
                (iarray_local ()),
Iarray.find_map_local_input ~f:(fun x -> if rem x 2. = 0.5
                          then Some ((globalize_float x -. 0.5) /. 2.)
                          else None)
                (ifarray_local ());;
[%%expect{|
- : int option * float option = (Some 1, Some 1.)
|}];;

Iarray.find_map_local_input ~f:(fun x -> if x mod 7 = 0
                          then Some (x / 7)
                          else None)
                (iarray_local ()),
Iarray.find_map_local_input ~f:(fun x -> if rem x 7. = 0.5
                          then Some ((globalize_float x -. 0.5) /. 7.)
                          else None)
                (ifarray_local ());;
[%%expect{|
- : int option * float option = (None, None)
|}];;

globalize_int_option
  (Iarray.find_map_local_output ~f:(fun x -> if x mod 2 = 0
                          then Some (x / 2)
                          else None) iarray),
globalize_float_option
  (Iarray.find_map_local_output ~f:(fun x -> if rem x 2. = 0.5
                          then exclave_ Some ((x -. 0.5) /. 2.)
                          else None) ifarray);;
[%%expect{|
- : int option * float option = (Some 1, Some 1.)
|}];;

globalize_int_option
  (Iarray.find_map_local_output ~f:(fun x -> if x mod 7 = 0
                          then Some (x / 7)
                          else None) iarray),
globalize_float_option
  (Iarray.find_map_local_output ~f:(fun x -> if rem x 7. = 0.5
                          then exclave_ Some ((x -. 0.5) /. 7.)
                          else None) ifarray);;
[%%expect{|
- : int option * float option = (None, None)
|}];;

Iarray.split [: 1, "a"; 2, "b"; 3, "c" :];;
[%%expect{|
- : int iarray * string iarray = ([:1; 2; 3:], [:"a"; "b"; "c":])
|}];;

Iarray.split [::];;
[%%expect{|
- : 'a iarray * 'b iarray = ([::], [::])
|}];;

let is, ss = Iarray.split_local (stack_ [: 1, "a"; 2, "b"; 3, "c" :]) in
globalize_int_iarray is, Iarray.map_local_input ~f:globalize_string ss;;
[%%expect{|
- : int iarray * string iarray = ([:1; 2; 3:], [:"a"; "b"; "c":])
|}];;

let is, ss = Iarray.split_local (stack_ [::]) in
globalize_int_iarray is, Iarray.map_local_input ~f:globalize_string ss;;
[%%expect{|
- : int iarray * string iarray = ([::], [::])
|}];;

Iarray.combine iarray ifarray;;
[%%expect{|
- : (int * float) iarray =
[:(1, 1.5); (2, 2.5); (3, 3.5); (4, 4.5); (5, 5.5):]
|}];;

Iarray.combine [::] [::];;
[%%expect{|
- : ('a * 'b) iarray = [::]
|}];;

Iarray.combine iarray [: "wrong length" :];;
[%%expect{|
Exception: Invalid_argument "Iarray.combine".
|}];;

globalize_int_float_iarray
  (Iarray.combine_local (iarray_local ()) (ifarray_local ()));;
[%%expect{|
- : (int * float) iarray =
[:(1, 1.5); (2, 2.5); (3, 3.5); (4, 4.5); (5, 5.5):]
|}];;

globalize_int_float_iarray (Iarray.combine_local [::] [::]);;
[%%expect{|
- : (int * float) iarray = [::]
|}];;

globalize_int_float_iarray (Iarray.combine_local iarray [: 1.5 :]);;
[%%expect{|
Exception: Invalid_argument "Iarray.combine_local".
|}];;

Iarray.sort ~cmp:(Fun.flip Int.compare) iarray,
Iarray.sort ~cmp:(Fun.flip Float.compare) ifarray;;
[%%expect{|
- : int iarray * Float.t iarray =
([:5; 4; 3; 2; 1:], [:5.5; 4.5; 3.5; 2.5; 1.5:])
|}];;

Iarray.stable_sort ~cmp:(Fun.flip Int.compare) iarray,
Iarray.stable_sort ~cmp:(Fun.flip Float.compare) ifarray;;
[%%expect{|
- : int iarray * Float.t iarray =
([:5; 4; 3; 2; 1:], [:5.5; 4.5; 3.5; 2.5; 1.5:])
|}];;

(* Check stability *)
Iarray.stable_sort
  ~cmp:(fun s1 s2 -> Int.compare (String.length s1) (String.length s2))
  [: "zero"; "one"; "two"; "three"; "four";
     "five"; "six"; "seven"; "eight"; "nine";
     "ten" :];;
[%%expect{|
- : String.t iarray =
[:"one"; "two"; "six"; "ten"; "zero"; "four"; "five"; "nine"; "three";
  "seven"; "eight":]
|}];;

Iarray.fast_sort ~cmp:(Fun.flip Int.compare) iarray,
Iarray.fast_sort ~cmp:(Fun.flip Float.compare) ifarray;;
[%%expect{|
- : int iarray * Float.t iarray =
([:5; 4; 3; 2; 1:], [:5.5; 4.5; 3.5; 2.5; 1.5:])
|}];;

Iarray.to_seq iarray |> List.of_seq;;
[%%expect{|
- : int list = [1; 2; 3; 4; 5]
|}];;

Iarray.to_seqi ifarray |> List.of_seq;;
[%%expect{|
- : (int * float) list = [(0, 1.5); (1, 2.5); (2, 3.5); (3, 4.5); (4, 5.5)]
|}];;

["hello"; "world"] |> List.to_seq |> Iarray.of_seq;;
[%%expect{|
- : string iarray = [:"hello"; "world":]
|}];;

(** Confirm that we haven't edited the immutable arrays, and that editing
    mutable siblings or copies does nothing *)

Array.fill marray 0 3 0;
marray;;
[%%expect{|
- : int array = [|0; 0; 0; 4; 5|]
|}];;

Array.fill (Iarray.to_array iarray) 3 2 10;
iarray;;
[%%expect{|
- : int iarray = [:1; 2; 3; 4; 5:]
|}];;

Array.fill mfarray 3 2 0.;
mfarray;;
[%%expect{|
- : float array = [|1.5; 2.5; 3.5; 0.; 0.|]
|}];;

Array.fill (Iarray.to_array ifarray) 0 3 10.;
ifarray;;
[%%expect{|
- : float iarray = [:1.5; 2.5; 3.5; 4.5; 5.5:]
|}];;

(* Confirm that nothing has changed *)

iarray;;
[%%expect{|
- : int iarray = [:1; 2; 3; 4; 5:]
|}];;

ifarray;;
[%%expect{|
- : float iarray = [:1.5; 2.5; 3.5; 4.5; 5.5:]
|}];;

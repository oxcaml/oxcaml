(* TEST
   flags += "-extension-universe alpha ";
   flags += "-dlambda -dno-unique-ids";
   expect;
*)

(* This file tests the lambda code that is generated for projections out of unique values.
   We need to ensure that if an allocation is used uniquely, all projections out of
   this allocation happen before the unique use and are not pushed down beyond that point.
*)

type record = { x : string; y : string @@ many aliased }
[%%expect{|
0
type record = { x : string; y : string @@ many aliased; }
|}]

let aliased_use x = x
[%%expect{|
(let (aliased_use = (function {nlocal = 0} x? x))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use))
val aliased_use : 'a -> 'a = <fun>
|}]

let unique_use (x @ unique) = x
[%%expect{|
(let (unique_use = (function {nlocal = 0} x? x))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use))
val unique_use : 'a @ unique -> 'a = <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased =
     (function {nlocal = 0} r[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y = (field_imm 1 r)
          r =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use r))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r y))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased))
val proj_aliased : record -> record * string = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique =
     (function {nlocal = 0} r[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y = (field_mut 1 r)
          r =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use r))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r y))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique))
val proj_unique : record @ unique -> record * string = <fun>
|}]

(* This output would be unsound if [aliased_use] was able to overwrite [r]
   because the [field_imm 1 r] read happens after calling [aliased_use]. *)
let match_aliased r =
  match r with
  | { y } ->
    let r = aliased_use r in
    (r, y)
[%%expect{|
(let
  (aliased_use =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased =
     (function {nlocal = 0} r[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (r =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use r))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r
           (field_imm 1 r)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased))
val match_aliased : record -> record * string = <fun>
|}]

(* This is sound since we bind [y] before the [unique_use] *)
let match_unique r =
  match r with
  | { y } ->
    let r = unique_use r in
    (r, y)
[%%expect{|
(let
  (unique_use =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique =
     (function {nlocal = 0} r[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y =o? (field_mut 1 r)
          r =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use r))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r y))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique))
val match_unique : record @ unique -> record * string = <fun>
|}]

(* Similarly, this would be unsound since Lambda performs a mini ANF pass. *)
let match_mini_anf_aliased r =
  let y, _ =
    match r with
    | { y } -> (y, 1)
  in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased =
     (function {nlocal = 0} r[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (*match* =[value<int>] 1
          r =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use r))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r
           (field_imm 1 r)))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased))
val match_mini_anf_aliased : record -> record * string = <fun>
|}]

(* This is sound since we bind [y] before the [unique_use] *)
let match_mini_anf_unique r =
  let y, _ =
    match r with
    | { y } -> (y, 1)
  in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique =
     (function {nlocal = 0} r[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y =o? (field_mut 1 r)
          *match* =[value<int>] 1
          r =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use r))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r y))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique))
val match_mini_anf_unique : record @ unique -> record * string = <fun>
|}]

let match_anf_aliased r =
  let y, _ =
    match r with
    | { y } when y == "" -> (y, 0)
    | { y } -> (y, 1)
  in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased =
     (function {nlocal = 0} r[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y =a? (field_imm 1 r))
           (if (%eq y "") (let (*match* =[value<int>] 0) (exit 21 y))
             (let (*match* =[value<int>] 1) (exit 21 (field_imm 1 r)))))
        with (21 y)
         (let
           (r =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply aliased_use r))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r y)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased))
val match_anf_aliased : record -> record * string = <fun>
|}]

(* This is sound since we bind [y] using [field_mut] *)
let match_anf_unique r =
  let y, _ =
    match r with
    | { y } when y == "" -> (y, 0)
    | { y } -> (y, 1)
  in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique =
     (function {nlocal = 0} r[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y =o? (field_mut 1 r))
           (if (%eq y "") (let (*match* =[value<int>] 0) (exit 29 y))
             (let (y =o? (field_mut 1 r) *match* =[value<int>] 1)
               (exit 29 y))))
        with (29 y)
         (let
           (r =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply unique_use r))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r y)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique" match_anf_unique))
val match_anf_unique : record @ unique -> record * string = <fun>
|}]

type tree =
  | Leaf
  | Node of { l : tree; x : int; r : tree }
[%%expect{|
0
type tree = Leaf | Node of { l : tree; x : int; r : tree; }
|}]

(* This output would be unsound with overwriting:
   If we naively replaced makeblock with reuseblock,
   then we would first overwrite r to have left child lr.
   But then, the overwrite of l still has to read the left child of r
   (as field_imm 0 *match*/329). But this value has been overwritten and so in fact,
   this code drops the rl and sets lr to be the inner child of both l and r.
*)
let swap_inner (t : tree) =
  match t with
  | Node ({ l = Node ({ r = lr } as l); r = Node ({ l = rl } as r) } as t) ->
    Node { t with l = Node { l with r = rl; }; r = Node { r with l = lr; }}
  | _ -> t
[%%expect{|
(let
  (swap_inner =
     (function {nlocal = 0}
       t[value<
          (consts (0))
           (non_consts ([0:
                         value<
                          (consts (0)) (non_consts ([0: *, value<int>, *]))>,
                         value<int>,
                         value<
                          (consts (0)) (non_consts ([0: *, value<int>, *]))>]))>]
       : (consts (0))
          (non_consts ([0:
                        value<
                         (consts (0)) (non_consts ([0: *, value<int>, *]))>,
                        value<int>,
                        value<
                         (consts (0)) (non_consts ([0: *, value<int>, *]))>]))
       (catch
         (if t
           (let (*match* =a? (field_imm 0 t))
             (if *match*
               (let (*match* =a? (field_imm 2 t))
                 (if *match*
                   (makeblock 0 (value<
                                  (consts (0))
                                   (non_consts ([0:
                                                 value<
                                                  (consts (0))
                                                   (non_consts ([0: *,
                                                                 value<int>,
                                                                 *]))>,
                                                 value<int>,
                                                 value<
                                                  (consts (0))
                                                   (non_consts ([0: *,
                                                                 value<int>,
                                                                 *]))>]))>,
                     value<int>,value<
                                 (consts (0))
                                  (non_consts ([0:
                                                value<
                                                 (consts (0))
                                                  (non_consts ([0: *,
                                                                value<int>,
                                                                *]))>,
                                                value<int>,
                                                value<
                                                 (consts (0))
                                                  (non_consts ([0: *,
                                                                value<int>,
                                                                *]))>]))>)
                     (makeblock 0 (value<
                                    (consts (0))
                                     (non_consts ([0:
                                                   value<
                                                    (consts (0))
                                                     (non_consts ([0: *,
                                                                   value<int>,
                                                                   *]))>,
                                                   value<int>,
                                                   value<
                                                    (consts (0))
                                                     (non_consts ([0: *,
                                                                   value<int>,
                                                                   *]))>]))>,
                       value<int>,value<
                                   (consts (0))
                                    (non_consts ([0:
                                                  value<
                                                   (consts (0))
                                                    (non_consts ([0: *,
                                                                  value<int>,
                                                                  *]))>,
                                                  value<int>,
                                                  value<
                                                   (consts (0))
                                                    (non_consts ([0: *,
                                                                  value<int>,
                                                                  *]))>]))>)
                       (field_imm 0 *match*) (field_int 1 *match*)
                       (field_imm 0 *match*))
                     (field_int 1 t)
                     (makeblock 0 (value<
                                    (consts (0))
                                     (non_consts ([0:
                                                   value<
                                                    (consts (0))
                                                     (non_consts ([0: *,
                                                                   value<int>,
                                                                   *]))>,
                                                   value<int>,
                                                   value<
                                                    (consts (0))
                                                     (non_consts ([0: *,
                                                                   value<int>,
                                                                   *]))>]))>,
                       value<int>,value<
                                   (consts (0))
                                    (non_consts ([0:
                                                  value<
                                                   (consts (0))
                                                    (non_consts ([0: *,
                                                                  value<int>,
                                                                  *]))>,
                                                  value<int>,
                                                  value<
                                                   (consts (0))
                                                    (non_consts ([0: *,
                                                                  value<int>,
                                                                  *]))>]))>)
                       (field_imm 2 *match*) (field_int 1 *match*)
                       (field_imm 2 *match*)))
                   (exit 36)))
               (exit 36)))
           (exit 36))
        with (36) t)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner))
val swap_inner : tree -> tree = <fun>
|}]

(* CR uniqueness: Update this test once overwriting is fully implemented.
   let swap_inner (t : tree) =
   match t with
   | Node { l = Node { r = lr } as l; r = Node { l = rl } as r } as t ->
   overwrite_ t with
   Node { l = overwrite_ l with Node { r = rl; };
   r = overwrite_ r with Node { l = lr; }}
   | _ -> t
   [%%expect{|

   |}]
*)

(***********************)
(* Barriers for guards *)

let match_guard r =
  match r with
  | { y } when String.equal y "" ->
    let r = aliased_use r in
    (r, y)
  | { y } ->
    let r = unique_use r in
    (r, y)
[%%expect{|
(let
  (unique_use =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard =
     (function {nlocal = 0} r[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let (y =o? (field_mut 1 r))
         (if (caml_string_equal y "")
           (let
             (r =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply aliased_use r))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r
               y))
           (let
             (y =o? (field_mut 1 r)
              r =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply unique_use r))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r
               y))))))
  (apply (field_imm 1 (global Toploop!)) "match_guard" match_guard))
val match_guard : record @ unique -> record * string = <fun>
|}]

let match_guard_unique (r @ unique) =
  match r with
  | { y } when String.equal ((unique_use r).x) "" -> y
  | _ -> ""
[%%expect{|
Line 3, characters 41-42:
3 |   | { y } when String.equal ((unique_use r).x) "" -> y
                                             ^
Error: This value is used here as unique, but it is also being read from at:
Line 3, characters 4-9:
3 |   | { y } when String.equal ((unique_use r).x) "" -> y
        ^^^^^

|}]

(********************************************)
(* Global allocations in overwritten fields *)

type option_record = { x : string option; y : string option }
[%%expect{|
0
type option_record = { x : string option; y : string option; }
|}]

let check_heap_alloc_in_overwrite (r : option_record @ unique) =
  overwrite_ r with { x = Some "" }
[%%expect{|
Line 2, characters 2-35:
2 |   overwrite_ r with { x = Some "" }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
>> Fatal error: Location.todo_overwrite_not_implemented
Uncaught exception: Misc.Fatal_error

|}]

let check_heap_alloc_in_overwrite (r : option_record @ local unique) =
  overwrite_ r with { x = Some "" }
[%%expect{|
Line 2, characters 2-35:
2 |   overwrite_ r with { x = Some "" }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
>> Fatal error: Location.todo_overwrite_not_implemented
Uncaught exception: Misc.Fatal_error

|}]

(*******************************)
(* Overwrite of mutable fields *)

type mutable_record = { mutable x : string; y : string }
[%%expect{|
0
type mutable_record = { mutable x : string; y : string; }
|}]

let update (r : mutable_record @ unique) =
  let x = overwrite_ r with { x = "foo" } in
  x.x
[%%expect{|
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { x = "foo" } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
>> Fatal error: Location.todo_overwrite_not_implemented
Uncaught exception: Misc.Fatal_error

|}]

let update (r : mutable_record @ unique) =
  let x = overwrite_ r with { y = "foo" } in
  x.x
[%%expect{|
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { y = "foo" } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
>> Fatal error: Location.todo_overwrite_not_implemented
Uncaught exception: Misc.Fatal_error

|}]

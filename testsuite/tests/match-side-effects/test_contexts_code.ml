(* TEST
 readonly_files = "contexts_1.ml contexts_2.ml contexts_3.ml";
 flags = "-dsource -dlambda";
 stack-allocation;
 expect;
*)

#use "contexts_1.ml";;
(* Notice that (field_mut 1 input) occurs twice, it
   is evaluated once in the 'false' branch and once in the 'true'
   branch. The compiler assumes that its static knowledge about the
   first read (it cannot be a [Right] as we already matched against it
   and failed) also applies to the second read, which is unsound.
*)
[%%expect {|

#use  "contexts_1.ml";;

type u = {
  a: bool ;
  mutable b: (bool, int) Either.t };;
0
type u = { a : bool; mutable b : (bool, int) Either.t; }

let example_1 () =
  let input = { a = true; b = (Either.Left true) } in
  match input with
  | { a = false; b = _ } -> Result.Error 1
  | { a = _; b = Either.Right _ } -> Result.Error 2
  | { a = _; b = _ } when input.b <- (Either.Right 3); false ->
      Result.Error 3
  | { a = true; b = Either.Left y } -> Result.Ok y;;
(let
  (example_1/309 =
     (function {nlocal = 0} param/333[value<int>]
       : (consts ()) (non_consts ([1: ?] [0: ?]))
       (region
         (let
           (input/311 =
              (makelocalmutable 0 (value<int>,value<
                                               (consts ())
                                                (non_consts ([1: ?] [0: ?]))>)
                1 [0: 1]))
           (if (field_int 0 input/311)
             (let (*match*/336 =o? (field_mut 1 input/311))
               (switch* *match*/336
                case tag 0:
                 (if (seq (setfield_ptr(maybe-stack) 1 input/311 [1: 3]) 0)
                   [1: 3]
                   (let (*match*/338 =o? (field_mut 1 input/311))
                     (makeblock 0 (value<int>) (field_imm 0 *match*/338))))
                case tag 1: [1: 2]))
             [1: 1])))))
  (apply (field_imm 1 (global Toploop!)) "example_1" example_1/309))
val example_1 : unit -> (bool, int) Result.t = <fun>
|}]

#use "contexts_2.ml";;
[%%expect {|

#use  "contexts_2.ml";;

type 'a myref = {
  mutable mut: 'a };;
0
type 'a myref = { mutable mut : 'a; }

type u = {
  a: bool ;
  b: (bool, int) Either.t myref };;
0
type u = { a : bool; b : (bool, int) Either.t myref; }

let example_2 () =
  let input = { a = true; b = { mut = (Either.Left true) } } in
  match input with
  | { a = false; b = _ } -> Result.Error 1
  | { a = _; b = { mut = Either.Right _ } } -> Result.Error 2
  | { a = _; b = _ } when (input.b).mut <- (Either.Right 3); false ->
      Result.Error 3
  | { a = true; b = { mut = Either.Left y } } -> Result.Ok y;;
(let
  (example_2/349 =
     (function {nlocal = 0} param/353[value<int>]
       : (consts ()) (non_consts ([1: ?] [0: ?]))
       (region
         (let
           (input/351 =[value<(consts ()) (non_consts ([0: value<int>, *]))>]
              (makelocalblock 0 (value<int>,*) 1
                (makelocalmutable 0 (value<
                                      (consts ()) (non_consts ([1: ?]
                                       [0: ?]))>)
                  [0: 1])))
           (if (field_int 0 input/351)
             (let (*match*/357 =o? (field_mut 0 (field_imm 1 input/351)))
               (switch* *match*/357
                case tag 0:
                 (if
                   (seq
                     (setfield_ptr(maybe-stack) 0 (field_imm 1 input/351)
                       [1: 3])
                     0)
                   [1: 3]
                   (let
                     (*match*/360 =o? (field_mut 0 (field_imm 1 input/351)))
                     (makeblock 0 (value<int>) (field_imm 0 *match*/360))))
                case tag 1: [1: 2]))
             [1: 1])))))
  (apply (field_imm 1 (global Toploop!)) "example_2" example_2/349))
val example_2 : unit -> (bool, int) Result.t = <fun>
|}]

#use "contexts_3.ml";;
[%%expect {|

#use  "contexts_3.ml";;

type 'a myref = {
  mutable mut: 'a };;
0
type 'a myref = { mutable mut : 'a; }

type u = (bool * (bool, int) Either.t) myref;;
0
type u = (bool * (bool, int) Either.t) myref

let example_3 () =
  let input = { mut = (true, (Either.Left true)) } in
  match input with
  | { mut = (false, _) } -> Result.Error 1
  | { mut = (_, Either.Right _) } -> Result.Error 2
  | { mut = (_, _) } when input.mut <- (true, (Either.Right 3)); false ->
      Result.Error 3
  | { mut = (true, Either.Left y) } -> Result.Ok y;;
(let
  (example_3/368 =
     (function {nlocal = 0} param/372[value<int>]
       : (consts ()) (non_consts ([1: ?] [0: ?]))
       (region
         (let
           (input/370 =mut[value<
                            (consts ())
                             (non_consts ([0: value<int>,
                                           value<
                                            (consts ()) (non_consts (
                                             [1: ?] [0: ?]))>]))>]
              [0: 1 [0: 1]]
            *match*/373 =o? *input/370)
           (if (field_imm 0 *match*/373)
             (switch* (field_imm 1 *match*/373)
              case tag 0:
               (if (seq (assign input/370 [0: 1 [1: 3]]) 0) [1: 3]
                 (makeblock 0 (value<int>)
                   (field_imm 0 (field_imm 1 *match*/373))))
              case tag 1: [1: 2])
             [1: 1])))))
  (apply (field_imm 1 (global Toploop!)) "example_3" example_3/368))
val example_3 : unit -> (bool, int) Result.t = <fun>
|}]

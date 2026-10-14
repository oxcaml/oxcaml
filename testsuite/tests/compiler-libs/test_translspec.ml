(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

(* The fragment of the expression language that [Translspec] accepts, and
   how [Untypespec] prints it back. Each expression is typed in an
   environment with the declarations below, with the variables [n], [b],
   [s], [xs], [r] and [g] bound as the parameters of a law would be. *)

let env =
  let prelude = {|
    type t = A | B of int | C of { x : int; y : bool }
    type r = { a : int; b : string }
    type ('a, 'b) pair = Pair of 'a * 'b
    type ur = #{ u : int; w : bool }
    exception E of int
    external ( + ) : int -> int -> int = "%addint"
    external ignore : 'a -> unit = "%ignore"
    let rec length = function [] -> 0 | _ :: l -> 1 + length l
    let f ?(opt = 0) ~lbl x = opt + lbl + x
    module M = struct let v = 1 end
  |} in
  let str = Parse.implementation (Lexing.from_string prelude) in
  let _, _, _, _, _, env = Typemod.type_structure (Lazy.force Env.initial) str
  in
  env
;;
[%%expect {|
val env : Env.t = <abstr>
|}]

let run_in env source =
  let wrapped =
    Printf.sprintf
      "fun (n : int) (b : bool) (s : string) (xs : int list) (r : r) \
       (g : int -> int) -> (%s)"
      source
  in
  let exp = Parse.expression (Lexing.from_string wrapped) in
  match (Typecore.type_expression env exp).exp_desc with
  | Texp_function { params; body = Tfunction_body body; _ } ->
      let bound =
        List.fold_left
          (fun bound (fp : Typedtree.function_param) ->
             Ident.Set.add fp.fp_param bound)
          Ident.Set.empty params
      in
      begin match Translspec.expression ~bound body with
      | spec ->
          let printed =
            Format.asprintf "%a" Pprintast.expression
              (Untypespec.expression ~lident_of_path:Untypespec.lident_of_path
                 ~annotate:(fun _ -> None) spec)
          in
          if String.contains printed '\n'
          then Format.printf "%s  ==>@\n%s@." source printed
          else Format.printf "%s  ==>  %s@." source printed
      | exception Translspec.Error (_, err) ->
          Format.printf "%s  ==>  Error: %a@." source
            (Format_doc.compat Translspec.report_error) err
      end
  | _ -> assert false
  | exception exn -> Location.report_exception Format.std_formatter exn
;;
let run = run_in env
[%%expect {|
val run_in : Env.t -> string -> unit = <fun>
val run : string -> unit = <fun>
|}]

(* Constants *)

let () = List.iter run
  ["1"; "'c'"; "\"s\""; "{s|quoted|s}"; "1.5"; "1l"; "1L"; "1n"; "1s"; "1S";
   "#1.5"; "#1.5s"; "#1l"; "#1L"; "#1n"; "#1m"; "#1s"; "#1S"; "#()"; "#true"]
[%%expect {|
1  ==>  1
'c'  ==>  'c'
"s"  ==>  "s"
{s|quoted|s}  ==>  {s|quoted|s}
1.5  ==>  1.5
1l  ==>  1l
1L  ==>  1L
1n  ==>  1n
1s  ==>  1s
1S  ==>  1S
#1.5  ==>  #1.5
#1.5s  ==>  #1.5s
#1l  ==>  #1l
#1L  ==>  #1L
#1n  ==>  #1n
#1m  ==>  #1m
#1s  ==>  #1s
#1S  ==>  #1S
#()  ==>  #()
#true  ==>  #true
|}]

(* Variables and globals *)

let () = List.iter run ["n"; "g n"; "M.v"; "length xs"; "f ~lbl:1 2"]
[%%expect {|
n  ==>  n
g n  ==>  g n
M.v  ==>  M.v
length xs  ==>  length xs
f ~lbl:1 2  ==>  f ?opt:None ~lbl:1 2
|}]

(* Applications. The [None] the type checker supplies for an omitted optional
   argument of a total application is printed like an explicit one. The
   argument omitted by a partial application is not printed. *)

let () = List.iter run
  ["f ~lbl:1 2"; "f ~opt:1 ~lbl:1 2"; "f ?opt:None ~lbl:1 2";
   "f ?opt:(Some n) ~lbl:1 2"; "f ~lbl:1"; "(f ~lbl:1) 2"]
[%%expect {|
f ~lbl:1 2  ==>  f ?opt:None ~lbl:1 2
f ~opt:1 ~lbl:1 2  ==>  f ?opt:(Some 1) ~lbl:1 2
f ?opt:None ~lbl:1 2  ==>  f ?opt:None ~lbl:1 2
f ?opt:(Some n) ~lbl:1 2  ==>  f ?opt:(Some n) ~lbl:1 2
f ~lbl:1  ==>  f ~lbl:1
(f ~lbl:1) 2  ==>  (f ~lbl:1) ?opt:None 2
|}]

(* Tuples, constructors, records, variants, lists *)

let () = List.iter run
  ["(n, b)";
   "(~x:n, b)";
   "#(n, b)";
   "A";
   "B n";
   "C { x = n; y = b }";
   "Pair (n, b)";
   "Some n";
   "None";
   "E n";
   "{ a = n; b = s }";
   "r.a";
   "{ r with a = n }";
   "#{ u = n; w = b }";
   "(#{ u = n; w = b }).#u";
   "#{ #{ u = n; w = b } with u = 0 }";
   "`A";
   "`B n";
   "[]";
   "n :: xs";
   "[n; n]"]
[%%expect {|
(n, b)  ==>  (n, b)
(~x:n, b)  ==>  (~x:n, b)
#(n, b)  ==>  #(n, b)
A  ==>  A
B n  ==>  B n
C { x = n; y = b }  ==>  C { x = n; y = b }
Pair (n, b)  ==>  Pair (n, b)
Some n  ==>  Some n
None  ==>  None
E n  ==>  E n
{ a = n; b = s }  ==>  { a = n; b = s }
r.a  ==>  r.a
{ r with a = n }  ==>  { r with a = n }
#{ u = n; w = b }  ==>  #{ u = n; w = b }
(#{ u = n; w = b }).#u  ==>  #{ u = n; w = b }.#u
#{ #{ u = n; w = b } with u = 0 }  ==>  #{ #{ u = n; w = b } with u = 0 }
`A  ==>  `A
`B n  ==>  `B n
[]  ==>  []
n :: xs  ==>  n :: xs
[n; n]  ==>  [n; n]
|}]

(* Control *)

let () = List.iter run
  ["if b then n else 0";
   "if b then ()";
   "ignore n; b";
   "assert b";
   "assert false"]
[%%expect {|
if b then n else 0  ==>  if b then n else 0
if b then ()  ==>  if b then ()
ignore n; b  ==>  ignore n; b
assert b  ==>  assert b
assert false  ==>  assert false
|}]

(* Lazy values and extension constructors *)

let () = List.iter run
  ["lazy n";
   "[%extension_constructor E]"]
[%%expect {|
lazy n  ==>  lazy n
[%extension_constructor E]  ==>  [%ocaml.extension_constructor E]
|}]

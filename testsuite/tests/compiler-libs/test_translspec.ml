(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

(* The fragment of the expression language that [Translspec] accepts, and
   how [Untypespec] prints it back. Each expression is typed in an
   environment with the declarations below, with the variables [n], [b],
   [s], [xs], [r], [t] and [g] bound as the parameters of a law would be. *)

let env =
  let prelude = {|
    type t = A | B of int | C of { x : int; y : bool }
    type r = { a : int; b : string }
    type ('a, 'b) pair = Pair of 'a * 'b
    type ex = Ex : 'a -> ex
    type m = { mutable c : int }
    type ur = #{ u : int; w : bool }
    type ('a, 'b) eq = Refl : ('a, 'a) eq
    exception E of int
    exception Failure of string
    external ( + ) : int -> int -> int = "%addint"
    external ( - ) : int -> int -> int = "%subint"
    external ( = ) : 'a -> 'a -> bool = "%equal"
    external ( > ) : 'a -> 'a -> bool = "%greaterthan"
    external ( && ) : bool -> bool -> bool = "%sequand"
    external ( || ) : bool -> bool -> bool = "%sequor"
    external not : bool -> bool = "%boolnot"
    external ignore : 'a -> unit = "%ignore"
    external raise : exn -> 'a = "%raise"
    external line : int = "%loc_LINE"
    external pos : string * int * int * int = "%loc_POS"
    let rec length = function [] -> 0 | _ :: l -> 1 + length l
    let hd = function x :: _ -> x | [] -> raise (Failure "hd")
    let is_some = function Some _ -> true | None -> false
    let f ?(opt = 0) ~lbl x = opt + lbl + x
    let ( let* ) x k = k x
    module M = struct let v = 1 end
    module type S = sig val v : int end
    module F (X : S) = struct
      type t = A
      let v = X.v
      exception Applied
    end
  |} in
  let str = Parse.implementation (Lexing.from_string prelude) in
  let _, _, _, _, _, env = Typemod.type_structure (Lazy.force Env.initial) str
  in
  env
;;
let () = Language_extension.enable Comprehensions ()
[%%expect {|
val env : Env.t = <abstr>
|}]

let run_in env source =
  let wrapped =
    Printf.sprintf
      "fun (n : int) (b : bool) (s : string) (xs : int list) (r : r) (t : t) \
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
  ["(n, b)"; "(~x:n, b)"; "#(n, b)";
   "A"; "B n"; "C { x = n; y = b }"; "Pair (n, b)"; "Some n"; "None"; "E n";
   "{ a = n; b = s }"; "r.a"; "{ r with a = n }";
   "match t with C c -> c.x | A | B _ -> 0";
   "#{ u = n; w = b }"; "(#{ u = n; w = b }).#u";
   "#{ #{ u = n; w = b } with u = 0 }";
   "match #{ u = n; w = b } with #{ u; _ } -> u";
   "`A"; "`B n";
   "[]"; "n :: xs"; "[n; n]"]
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
match t with C c -> c.x | A | B _ -> 0  ==>  match t with | C c -> c.x | A | B _ -> 0
#{ u = n; w = b }  ==>  #{ u = n; w = b }
(#{ u = n; w = b }).#u  ==>  #{ u = n; w = b }.#u
#{ #{ u = n; w = b } with u = 0 }  ==>  #{ #{ u = n; w = b } with u = 0 }
match #{ u = n; w = b } with #{ u; _ } -> u  ==>  match #{ u = n; w = b } with | #{ u;_} -> u
`A  ==>  `A
`B n  ==>  `B n
[]  ==>  []
n :: xs  ==>  n :: xs
[n; n]  ==>  [n; n]
|}]

(* Control *)

let () = List.iter run
  ["if b then n else 0"; "if b then ()";
   "match t with A -> 0 | B n -> n | C { x; _ } -> x";
   "match t with B n when n > 0 -> true | A | B _ -> false | C _ -> b";
   "match hd xs with n -> n | exception Failure _ -> 0";
   "match (n, b) with (0, true) | (1, false) -> true | _ -> false";
   "match xs with (x :: _) as l -> x + length l | [] -> 0";
   "match r with { a; b = \"\" } -> a | { a = 0; _ } -> 0 | _ -> 1";
   "try hd xs with Failure _ -> 0 | E n -> n";
   "ignore n; b"; "assert b"; "assert false"]
[%%expect {|
if b then n else 0  ==>  if b then n else 0
if b then ()  ==>  if b then ()
match t with A -> 0 | B n -> n | C { x; _ } -> x  ==>  match t with | A -> 0 | B n -> n | C { x;_} -> x
match t with B n when n > 0 -> true | A | B _ -> false | C _ -> b  ==>  match t with | B n when n > 0 -> true | A | B _ -> false | C _ -> b
match hd xs with n -> n | exception Failure _ -> 0  ==>  match hd xs with | n -> n | exception Failure _ -> 0
match (n, b) with (0, true) | (1, false) -> true | _ -> false  ==>  match (n, b) with | (0, true) | (1, false) -> true | _ -> false
match xs with (x :: _) as l -> x + length l | [] -> 0  ==>  match xs with | x::_ as l -> x + (length l) | [] -> 0
match r with { a; b = "" } -> a | { a = 0; _ } -> 0 | _ -> 1  ==>  match r with | { a; b = "" } -> a | { a = 0;_} -> 0 | _ -> 1
try hd xs with Failure _ -> 0 | E n -> n  ==>  try hd xs with | Failure _ -> 0 | E n -> n
ignore n; b  ==>  ignore n; b
assert b  ==>  assert b
assert false  ==>  assert false
|}]

(* Bindings *)

let () = List.iter run
  ["let x = n in x"; "let x, y = (n, b) in if y then x else 0";
   "let rec even k = k = 0 || odd (k - 1) and odd k = k > 0 && even (k - 1) \
    in even n";
   "fun x -> x + n"; "fun ~lbl ?(opt = 0) x -> lbl + opt + x";
   "fun ?opt x -> match opt with Some o -> o + x | None -> x";
   "function A -> true | B _ | C _ -> false";
   "fun (x : int) : int -> x"]
[%%expect {|
let x = n in x  ==>  let x = n in x
let x, y = (n, b) in if y then x else 0  ==>  let (x, y) = (n, b) in if y then x else 0
let rec even k = k = 0 || odd (k - 1) and odd k = k > 0 && even (k - 1) in even n  ==>
let rec even k = (k = 0) || (odd (k - 1))
and odd k = (k > 0) && (even (k - 1)) in even n
fun x -> x + n  ==>  fun x -> x + n
fun ~lbl ?(opt = 0) x -> lbl + opt + x  ==>  fun ~lbl ?(opt= 0) x -> (lbl + opt) + x
fun ?opt x -> match opt with Some o -> o + x | None -> x  ==>  fun ?opt x -> match opt with | Some o -> o + x | None -> x
function A -> true | B _ | C _ -> false  ==>  function | A -> true | B _ | C _ -> false
fun (x : int) : int -> x  ==>  fun x -> x
|}]

(* Local opens of module paths are resolved away, and local modules bound
   to module paths are substituted away. *)

let () = List.iter run
  ["let open M in v"; "M.(v)"; "match n with M.(_) -> true";
   "let module L = M in L.v"; "let module L = F (M) in (L.A : L.t)"]
[%%expect {|
let open M in v  ==>  M.v
M.(v)  ==>  M.v
match n with M.(_) -> true  ==>  match n with | _ -> true
let module L = M in L.v  ==>  M.v
let module L = F (M) in (L.A : L.t)  ==>  A
|}]

(* Lazy values and extension constructors *)

let () = List.iter run
  ["lazy n"; "match lazy n with lazy _ -> true";
   "[%extension_constructor E]"]
[%%expect {|
lazy n  ==>  lazy n
match lazy n with lazy _ -> true  ==>  match lazy n with | (lazy _) -> true
[%extension_constructor E]  ==>  [%ocaml.extension_constructor E]
|}]

(* A signature open of a functor application gives the values paths
   through the application, which laws do not support. *)

let applied_env =
  let sg = Parse.interface (Lexing.from_string "open F (M)") in
  let sg =
    Typemod.type_interface ~sourcefile:"applied.mli"
      (Compilation_unit.of_string "Applied") env sg
  in
  sg.sig_final_env
let () = List.iter (run_in applied_env)
  ["v"; "raise Applied"; "[%extension_constructor Applied]"]
[%%expect {|
val applied_env : Env.t = <abstr>
v  ==>  Error: Laws do not support values through functor applications.
raise Applied  ==>  Error: Laws do not support values through functor applications.
[%extension_constructor Applied]  ==>  Error: Laws do not support values through functor applications.
|}]

let () = run "let module L = F (M) in L.v"
[%%expect {|
let module L = F (M) in L.v  ==>  Error: Laws do not support values through functor applications.
|}]

(* Rejected constructs *)

let () = List.iter run
  ["fun (type a) (x : a) -> x"; "(n :> int)";
   "let h ~(here : [%call_pos]) () = here in ignore (h ()); true";
   "match Ex n with Ex _ -> true";
   "let module L = struct let x = n end in L.x";
   "let open struct let v = n end in v"; "let open F (M) in v";
   "fun (e : (int, string) eq) -> match e with _ -> .";
   "let f : 'a. 'a -> 'a = fun x -> x in f n";
   "let rec f : 'a. int -> 'a list -> bool = fun k l -> k = 0 || f (k - 1) [l] \
    in f n [b]";
   "let exception F in true";
   "let* x = n in x"; "let mutable x = n in x";
   "[| n |]"; "match [| n |] with [| _ |] -> true | _ -> false";
   "[ x for x in xs ]"; "[| x for x = 0 to n |]";
   "{ c = n }.c <- n"; "while b do () done"; "for i = 0 to n do () done";
   "object end"; "(module M : S)"; "let (module L : S) = (module M) in L.v";
   "let _ = stack_ (n, b) in true"; "exclave_ true";
   "line"; "pos"; "[%src_pos]"]
[%%expect {|
fun (type a) (x : a) -> x  ==>  Error: Laws do not support locally abstract types.
(n :> int)  ==>  Error: Laws do not support coercions.
let h ~(here : [%call_pos]) () = here in ignore (h ()); true  ==>  Error: Laws do not support source position arguments.
match Ex n with Ex _ -> true  ==>  Error: Laws do not support constructors with existential types in patterns.
let module L = struct let x = n end in L.x  ==>  Error: Laws do not support local modules that are not module paths.
let open struct let v = n end in v  ==>  Error: Laws do not support local opens that are not of module paths.
let open F (M) in v  ==>  Error: Laws do not support local opens that are not of module paths.
fun (e : (int, string) eq) -> match e with _ -> .  ==>  Error: Laws do not support refutation cases.
let f : 'a. 'a -> 'a = fun x -> x in f n  ==>  Error: Laws do not support polymorphic type annotations.
let rec f : 'a. int -> 'a list -> bool = fun k l -> k = 0 || f (k - 1) [l] in f n [b]  ==>  Error: Laws do not support polymorphic type annotations.
let exception F in true  ==>  Error: Laws do not support local exceptions.
let* x = n in x  ==>  Error: Laws do not support binding operators.
let mutable x = n in x  ==>  Error: Laws do not support mutable variables.
[| n |]  ==>  Error: Laws do not support arrays.
match [| n |] with [| _ |] -> true | _ -> false  ==>  Error: Laws do not support arrays.
[ x for x in xs ]  ==>  Error: Laws do not support comprehensions.
[| x for x = 0 to n |]  ==>  Error: Laws do not support comprehensions.
{ c = n }.c <- n  ==>  Error: Laws do not support mutation.
while b do () done  ==>  Error: Laws do not support loops.
for i = 0 to n do () done  ==>  Error: Laws do not support loops.
object end  ==>  Error: Laws do not support objects.
(module M : S)  ==>  Error: Laws do not support first-class modules.
let (module L : S) = (module M) in L.v  ==>  Error: Laws do not support first-class modules.
let _ = stack_ (n, b) in true  ==>  Error: Laws do not support stack_.
exclave_ true  ==>  Error: Laws do not support exclave_.
line  ==>  Error: Laws do not support source locations.
pos  ==>  Error: Laws do not support source locations.
[%src_pos]  ==>  Error: Laws do not support source positions.
|}]

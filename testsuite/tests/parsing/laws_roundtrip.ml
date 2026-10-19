(* TEST
 flags = "-I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

(* The parse tree of a law prints with [Pprintast] to a source that parses
   back to the same tree. *)

let roundtrip source =
  let parse s = Parse.implementation (Lexing.from_string s) in
  let print str = Format.asprintf "%a" Pprintast.structure str in
  let tree str =
    Clflags.locations := false;
    Format.asprintf "%a" Printast.implementation str
  in
  let str = parse source in
  let printed = print str in
  let str' = parse printed in
  Format.printf "%s@." printed;
  if print str' <> printed then Format.printf "PRINTING IS NOT A FIXPOINT@.";
  if tree str' <> tree str then Format.printf "TREES DIFFER@."
;;
[%%expect {|
val roundtrip : string -> unit = <fun>
|}]

let () = roundtrip "law? trivial : true"
[%%expect {|
law? trivial : true
|}]

let () = roundtrip {|
  law? assoc (x : int) (y : int) (z : int) :
    x >= 0 ===> y >= 0 ===> x + (y + z) = (x + y) + z
|}
[%%expect {|
law? assoc (x : int) (y : int) (z : int) : x >= 0 ===> y >= 0 ===>
  (x + (y + z)) = ((x + y) + z)
|}]

(* Parameters with and without type annotations. *)

let () = roundtrip {|
  law? mixed x (y : int) z (t : 'a list) : x + y = z && t = []
|}
[%%expect {|
law? mixed x (y : int) z (t : 'a list) : ((x + y) = z) && (t = [])
|}]

(* The clauses are [expr]s: the ones that extend as far as possible are
   parenthesized. *)

let () = roundtrip {|
  law? clauses (a : bool) (n : int) :
    (let b = a in b) ===> (if a then n else 0) >= 0 ===>
    (match n with 0 -> true | _ -> a) ===> (ignore n; a) ===>
    (fun b -> b) a ===> (try a with _ -> false)
|}
[%%expect {|
law? clauses (a : bool) (n : int) : (let b = a in b) ===>
  (if a then n else 0) >= 0 ===> (match n with | 0 -> true | _ -> a) ===>
  (ignore n; a) ===> ((fun b -> b)) a ===> (try a with | _ -> false)
|}]

let () = roundtrip {|
  module type S = sig
    val f : int -> int
    law? f_id (x : int) : x >= 0 ===> f x = x [@@attr]
    law? polymorphic (xs : 'a list) : List.rev (List.rev xs) = xs
  end
  module F (X : S) = struct
    law? in_functor (x : int) : X.f x = x
  end
|}
[%%expect {|
module type S  =
  sig
    val f : int -> int
    law? f_id (x : int) : x >= 0 ===> (f x) = x[@@attr ]
    law? polymorphic (xs : 'a list) : (List.rev (List.rev xs)) = xs
  end
module F(X:S) = struct law? in_functor (x : int) : (X.f x) = x end
|}]

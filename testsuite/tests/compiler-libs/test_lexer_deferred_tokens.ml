(* TEST
 flags = "-I ${ocamlsrcdir}/parsing";
 include ocamlcommon;
 expect;
*)

(* [a#b] is lexed as [LIDENT a] with a deferred [HASH_SUFFIX]. A syntax
   error at [a] must not leave that token for the next parse. *)
let parse s =
  match Parse.implementation (Lexing.from_string s) with
  | _ -> "ok"
  | exception Syntaxerr.Error _ -> "syntax error"
;;
[%%expect{|
val parse : string -> string = <fun>
|}];;

let results = List.map parse ["module M = a#b"; "let y = 1"];;
[%%expect{|
val results : string list = ["syntax error"; "ok"]
|}];;

(* TEST
 include ocamlcommon;
*)

(* Parse -> pretty-print -> reparse round-trip (idempotency) test for the
   [thm_?] theorem declaration syntax, plus backward-compatibility checks
   confirming that [thm] (no underscore) is still a valid identifier while
   [thm_] is now a reserved keyword.

   For each input we parse it as a signature, pretty-print, reparse, and
   pretty-print again, checking that the two printed forms are identical
   (printer is a fixed point on a parseable program) and that the ASTs are
   equal modulo locations.  We print PASS / FAIL plus the canonical printed
   form so the reference documents how each construct round-trips. *)

let remove_locs =
  let open Ast_mapper in
  { default_mapper with location = (fun _ _ -> Location.none) }

let strip sg = remove_locs.Ast_mapper.signature remove_locs sg

let parse str = Parse.interface (Lexing.from_string str)

let print sg =
  Format.asprintf "%a" Pprintast.signature sg

let check name str =
  match parse str with
  | exception exn ->
      Printf.printf "%s: FAIL (cannot parse)\n  %s\n" name
        (Printexc.to_string exn)
  | ast1 ->
      let printed1 = print ast1 in
      (match parse printed1 with
       | exception exn ->
           Printf.printf "%s: FAIL (cannot reparse)\n  printed: %s\n  %s\n"
             name printed1 (Printexc.to_string exn)
       | ast2 ->
           let printed2 = print ast2 in
           let same_text = String.equal printed1 printed2 in
           let same_ast = strip ast1 = strip ast2 in
           if same_text && same_ast then
             Printf.printf "%s: PASS\n  %s\n" name printed1
           else
             Printf.printf
               "%s: FAIL (not idempotent; text=%b ast=%b)\n  1: %s\n  2: %s\n"
               name same_text same_ast printed1 printed2)

let () = print_string "== theorem round-trip ==\n"

(* 1. Round-trip of [thm_?] theorem syntax. *)
let () =
  check "sugar-spec" "thm_? sqrt_1 : {[ sqrt 1. = 1. ]}";
  check "refinement-arrow"
    "thm_? small : (x : float | x < 1.) -> {[ sqrt x > x ]}";
  check "plain-type" "thm_? t1 : int -> int";
  check "mixed"
    "val sqrt : (x : float | x >= 0.) -> (y : float | y >= 0.)\n\
     thm_? sqrt_1 : {[ sqrt 1. = 1. ]}\n\
     thm_? sqrt_small : (x : float | x < 1.) -> {[ sqrt x > x ]}\n\
     thm_? sqrt_big : (x : float | x > 1.) -> {[ sqrt x < x ]}"

let () = print_string "\n== backward compatibility ==\n"

(* 2. [thm] (no underscore) is still a usable identifier. *)
let () =
  let s = "let thm = 1" in
  match Parse.implementation (Lexing.from_string s) with
  | exception exn ->
      Printf.printf "ident-let-thm: FAIL (cannot parse) %s\n"
        (Printexc.to_string exn)
  | str -> Printf.printf "ident-let-thm: %s\n"
             (Format.asprintf "%a" Pprintast.structure str)

let () = check "ident-val-thm" "val thm : int"

let () = print_string "\n== keyword / parse errors ==\n"

(* Report whether parsing [str] (as an implementation) raises a parse
   error.  [thm_] is now a reserved keyword, so it cannot be used as an
   identifier; we expect a Syntaxerr error rather than a successful parse. *)
let parse_impl_fails name str =
  match Parse.implementation (Lexing.from_string str) with
  | exception Syntaxerr.Error _ ->
      Printf.printf "%s: PASS (syntax error as expected)\n" name
  | exception exn ->
      Printf.printf "%s: PASS (parse error: %s)\n" name
        (Printexc.to_string exn)
  | _ -> Printf.printf "%s: FAIL (parsed without error)\n" name

(* Likewise for signatures. *)
let parse_intf_fails name str =
  match parse str with
  | exception Syntaxerr.Error _ ->
      Printf.printf "%s: PASS (syntax error as expected)\n" name
  | exception exn ->
      Printf.printf "%s: PASS (parse error: %s)\n" name
        (Printexc.to_string exn)
  | _ -> Printf.printf "%s: FAIL (parsed without error)\n" name

(* 3a. [thm_] is reserved: it cannot be used as an identifier. *)
let () = parse_impl_fails "reserved-let-thm_" "let thm_ = 1"

(* 3b. The [?] is mandatory: [thm_ name : ...] without it is a parse
   error. *)
let () = parse_intf_fails "missing-question" "thm_ name : int"

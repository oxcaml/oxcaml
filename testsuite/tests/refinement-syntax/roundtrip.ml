(* TEST
 include ocamlcommon;
*)

(* Parse -> pretty-print -> reparse round-trip (idempotency) test for the
   refinement type syntax, plus backward-compatibility checks confirming
   that existing syntax still parses with its original meaning.

   For each input string we:
     1. parse it as a signature,
     2. pretty-print the parsed AST,
     3. reparse the printed output,
     4. pretty-print that again,
   and check that the two printed forms are identical (printer is a fixed
   point on a parseable program) and that the two ASTs are equal modulo
   locations.  We print PASS / FAIL plus the canonical printed form so the
   reference file documents how each construct round-trips. *)

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

let () = print_string "== refinement round-trip ==\n"

(* 1. Round-trip of refinement type syntax. *)
let () =
  check "named" "val f : (x : int | x > 0) -> int";
  check "anon" "val g : (int | true) -> int";
  check "anon-arrow" "val h : (int -> int | true) -> int";
  check "sugar" "val k : (x : float | x >= 0.) -> {[ k1 = k1 ]}";
  check "dependent-arrow" "val s : (x : float | x < 1.) -> {[ sqrt x > x ]}";
  check "nested-list" "val t : (int | true) list -> int";
  check "named-paren-arrow" "val u : (x : (int -> int) | true) -> int"

let () = print_string "\n== backward compatibility ==\n"

(* 2a. [[1];[2]] is a list of two singleton lists, not a refinement. *)
let () =
  let s = "let _ = [[1];[2]]" in
  match Parse.implementation (Lexing.from_string s) with
  | exception exn ->
      Printf.printf "list-of-lists: FAIL (cannot parse) %s\n"
        (Printexc.to_string exn)
  | str -> Printf.printf "list-of-lists: %s\n" (Format.asprintf "%a"
             Pprintast.structure str)

(* 2b. Polymorphic variants still parse as before. *)
let () =
  check "polyvar-simple" "type v = [ `A ]";
  check "polyvar-nested" "type w = [ [ `A ] | `B ]"

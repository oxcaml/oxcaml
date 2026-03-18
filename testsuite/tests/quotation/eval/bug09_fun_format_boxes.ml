(* TEST
  flags = "-extension runtime_metaprogramming";
  native;
*)

(* Bug 9: Fun printing has mismatched Format boxes.
   "fun x -> function | 0 -> true | _ -> false" should be formatted
   without garbled output from unbalanced format boxes. *)

#syntax quotations on

let print_narrow width e =
  let buf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buf in
  Format.pp_set_margin fmt width;
  Quote.print fmt e;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let () =
  (* params<>[] with Pfunction_cases: box A from "fun" is opened
     but never closed. This can cause garbled indentation when the
     formatter wraps lines. *)
  let s = print_narrow 30
    <[ fun x y z -> function
       | 0 -> x + y + z
       | n -> x + y + z + n ]> in
  Printf.printf "%s\n" s

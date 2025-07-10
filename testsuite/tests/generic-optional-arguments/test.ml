(* TEST
 flags = "-extension-universe alpha";
*)

(* Interface *)
module type S = sig
val concat : Stdlib.Option.?'sep:string -> string list -> string

val concat_2 : ?sep:string -> string list -> string
end

(* Implementation *)
module M : S = struct
let concat Stdlib.Option.?'(sep : string = " ") xs =
  String.concat sep xs
let concat_2 Stdlib.Option.?'(sep=" ") xs =
  String.concat sep xs
end

(* Usage *)
let default_concat ys = M.concat ys
let comma_concat zs = M.concat ~sep:"," zs
let chain_call Stdlib.Option.?'(sep : string option) arg =
  M.concat Stdlib.Option.?'sep arg

let chain_call_2 Stdlib.Option.?'(sep) arg =
  String.concat (match sep with None -> " " | Some s -> s) arg

let () =
  print_endline (default_concat ["x"; "y"; "z"]);
  print_endline (comma_concat ["x"; "y"; "z"]);
  print_endline (chain_call ~sep:"+" ["x"; "y"; "z"]);
  print_endline (chain_call ?sep:(Some "+") ["x"; "y"; "z"]);
  print_endline (chain_call_2 ?sep:(Some "+") ["x"; "y"; "z"]);
  (* CR generic-optional : The following two don't work yet.*)
  (*= print_endline (chain_call ~sep:("+" @ local) ["x"; "y"; "z"]);
  print_endline (chain_call_2 ?sep:(Some "+" @ local) ["x"; "y"; "z"]); *)
  ()

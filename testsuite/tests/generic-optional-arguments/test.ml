(* TEST
 flags = "-extension-universe alpha";
*)

(* Interface *)
module type S = sig
val concat : (?sep):string option -> string list -> string

val concat_2 : (?sep):string or_null -> string list -> string
end

(* Implementation *)
module M : S = struct
let concat (?(sep = " ") : string option) xs =
  String.concat sep xs
let concat_2 (?(sep = " ") : string or_null) xs =
  String.concat sep xs

let concat_3 (?sep : string or_null) xs =
  String.concat (
    match sep with
    | Null -> " "
    | This s -> s
  ) xs
end

(* Usage *)
let default_concat ys = M.concat ys
let comma_concat zs = M.concat ~sep:"," zs
let chain_call (?sep : string option) arg =
  (* CR generic-optional : also need to parse without the Module_name. prefix *)
  M.concat ?sep arg

let chain_call_2 ?sep arg =
  M.concat_2 ?sep:(
    match sep with
    | None -> Null
    | Some x -> This x
  ) arg

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

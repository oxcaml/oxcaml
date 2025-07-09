(* TEST
 flags = "-extension-universe alpha";


*)


(* Interface *)
module type S = sig
val concat : Stdlib.Option.?sep:string -> string list -> string
end

(* Implementation *)
module M : S = struct
let concat Stdlib.Option.?(sep : string <- " ") xs =
  String.concat sep xs
end

(* Usage *)
let default_concat ys = M.concat ys
let comma_concat zs = M.concat ~sep:(Some ",") zs
let chain_call Stdlib.Option.?(sep : string option) arg = M.concat Stdlib.Option.?sep arg

let () =
  print_endline (default_concat ["x"; "y"; "z"]);
  print_endline (comma_concat ["x"; "y"; "z"]);
  print_endline (chain_call ["x"; "y"; "z"]);
  ()

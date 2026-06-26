[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@warning "-66"]
module String = Misc.Stdlib.String

type value = int array

type t = { mutable callee_regs : value String.Map.t }

let create () = { callee_regs = String.Map.empty }

let reset t = t.callee_regs <- String.Map.empty

let merge src ~into:dst =
  let join key _v1 _v2 =
    Misc.fatal_errorf "Unexpected merge for %s" key
  in
  dst.callee_regs <- String.Map.union join dst.callee_regs src.callee_regs

let get_value (t : t) s = String.Map.find_opt s t.callee_regs

let set_value (t : t) s (v : value) =
  let f new_ old =
    if not (Option.is_none old)
    then Misc.fatal_errorf "Value of %s is already set" s;
    Some new_
  in
  t.callee_regs <- String.Map.update s (f v) t.callee_regs

module Raw = struct
  type entries = (string * int array) list

  type t = entries option

  let entries_to_map (e : entries) =
    List.fold_left (fun acc (k, v) -> String.Map.add k v acc) String.Map.empty e

  let print t =
    let print (name, v) =
      Printf.printf "\t\t%s = [%s]\n" name
        (String.concat "; "
           (List.init (Array.length v) (fun i -> string_of_int v.(i))))
    in
    Printf.printf "Callee register usage for leaf functions:\n";
    List.iter print t

  let print = function None -> () | Some t -> print t

  let decode_bitmask ~class_idx ~reg_name bitmask =
    let regs = ref [] in
    let b = ref bitmask in
    let bit_idx = ref 0 in
    while !b <> 0 do
      if !b land 1 <> 0
      then regs := reg_name ~class_idx ~bit_idx:!bit_idx :: !regs;
      b := !b lsr 1;
      incr bit_idx
    done;
    List.rev !regs

  let print_with_names ~class_name ~reg_name t =
    let print (fun_name, v) =
      Printf.printf "\t\t%s:\n" fun_name;
      Array.iteri
        (fun class_idx bitmask ->
          if bitmask <> 0
          then begin
            let reg_names = decode_bitmask ~class_idx ~reg_name bitmask in
            Printf.printf "\t\t\t%s: %s\n" (class_name class_idx)
              (String.concat " " reg_names)
          end)
        v
    in
    Printf.printf "Callee register usage for leaf functions:\n";
    List.iter print t

  let print_with_names ~class_name ~reg_name = function
    | None -> ()
    | Some t -> print_with_names ~class_name ~reg_name t
end

let to_raw (t : t) : Raw.t =
  if String.Map.is_empty t.callee_regs
  then None
  else Some (String.Map.bindings t.callee_regs)

let of_raw (t : Raw.t) : t =
  match t with
  | None -> create ()
  | Some t -> { callee_regs = Raw.entries_to_map t }

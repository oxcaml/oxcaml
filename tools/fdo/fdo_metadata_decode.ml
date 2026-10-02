(* See backend/fdo_metadata_encode.ml for the wire format. *)

module Hash = Fdo_counter.Hash

(* Keyed by code address. *)
module Address_tbl = Hashtbl.Make (struct
  type t = int64

  let equal = Int64.equal

  let hash address = Int64.to_int address land max_int
end)

type action =
  | Call of
      { stack : Fdo_counter.hashed;
        return_address : int64
      }
  | Tailcall of Fdo_counter.hashed
  | Normal of Fdo_counter.hashed
  | Jump of
      { target : int64;
        stack : Fdo_counter.hashed
      }
  | Reset

type t =
  { taken : action list Address_tbl.t;
    fallthrough : (int64 * action list) array;
    functions : (int64 * int64) array;
    bodies : Fdo_counter.Function_body_hash.t Hash.Tbl.t;
    names : string Hash.Tbl.t;
    annotations : (int * action) list list
        (** per function in metadata order: control bits and action *)
  }

let names t = t.names

let bodies t = t.bodies

let sorted table =
  let array = Array.of_seq (Address_tbl.to_seq table) in
  Array.sort (fun (a, _) (b, _) -> Int64.unsigned_compare a b) array;
  array

let parse data =
  let taken = Address_tbl.create 256 and fallthrough = Address_tbl.create 256 in
  let names = Hash.Tbl.create 256 and functions = Address_tbl.create 64 in
  let bodies = Hash.Tbl.create 256 in
  let annotations = ref [] in
  let pos = ref 0 in
  let fail fmt =
    Printf.ksprintf
      (fun msg -> failwith (".debug_fdo_metadata section: " ^ msg))
      fmt
  in
  let need n =
    if n < 0 || n > String.length data - !pos then fail "truncated section"
  in
  let byte () =
    need 1;
    let b = Char.code data.[!pos] in
    incr pos;
    b
  in
  let uleb () =
    let rec loop shift acc =
      let b = byte () in
      if shift > 56 || (shift = 56 && b land 0x7f > 63)
      then fail "ULEB128 integer too large";
      let acc = acc lor ((b land 0x7f) lsl shift) in
      if b land 0x80 = 0 then acc else loop (shift + 7) acc
    in
    loop 0 0
  in
  let u64 () =
    need 8;
    let v = String.get_int64_le data !pos in
    pos := !pos + 8;
    v
  in
  let int32 () =
    need 4;
    let v = String.get_int32_le data !pos in
    pos := !pos + 4;
    v
  in
  let hash () = Hash.of_int32 (int32 ()) in
  let stack previous ~shared =
    let suffix =
      if not shared
      then []
      else
        let rec drop n = function
          | stack when n = 0 -> stack
          | [] -> fail "shared stack drop exceeds previous depth"
          | _ :: rest -> drop (n - 1) rest
        in
        drop (uleb ()) !previous
    in
    let n = uleb () in
    if n > (String.length data - !pos) / 4 then fail "truncated hash array";
    let stack = List.init n (fun _ -> hash ()) @ suffix in
    if List.is_empty stack then fail "empty counting stack";
    previous := stack;
    stack
  in
  let action previous ~start ~address ~finish flags =
    let shared = flags land 32 <> 0 in
    let code = (flags lsr 2) land 7 in
    if (code = 0 || code = 1 || code = 4) && flags land 3 <> 2
    then fail "call or jump annotation must be taken-only";
    match code with
    | 0 ->
      let length = uleb () in
      if
        length = 0
        || Int64.compare (Int64.of_int length) (Int64.sub finish address) > 0
      then fail "call instruction outside function";
      let return_address = Int64.add address (Int64.of_int length) in
      Call { return_address; stack = stack previous ~shared }
    | 1 -> Tailcall (stack previous ~shared)
    | 2 -> Normal (stack previous ~shared)
    | 3 ->
      if shared then fail "sharing bit on a non-counting action";
      Reset
    | 4 ->
      let offset = uleb () in
      if Int64.compare (Int64.of_int offset) (Int64.sub finish start) >= 0
      then fail "jump target outside function";
      let target = Int64.add start (Int64.of_int offset) in
      Jump { target; stack = stack previous ~shared }
    | _ -> fail "invalid action"
  in
  let add table address op =
    let previous =
      Option.value (Address_tbl.find_opt table address) ~default:[]
    in
    Address_tbl.replace table address (op :: previous)
  in
  while !pos < String.length data do
    need 4;
    if not (String.equal (String.sub data !pos 4) "FDOM") then fail "bad magic";
    pos := !pos + 4;
    (match uleb () with 17 -> () | v -> fail "unsupported version %d" v);
    let num_functions = uleb () in
    for _ = 1 to num_functions do
      let start = u64 () in
      let length = uleb () in
      if Int64.compare start 0L < 0 || length <= 0
      then fail "invalid function range";
      let finish = Int64.add start (Int64.of_int length) in
      if Int64.compare finish start <= 0 then fail "function range overflow";
      Address_tbl.replace functions start finish;
      let entries = uleb () in
      let previous = ref [] in
      let annotated = ref [] in
      for _ = 1 to entries do
        let offset = uleb () in
        if offset >= length then fail "annotation outside function";
        let flags = byte () in
        if flags lsr 6 <> 0 || flags land 3 = 0 then fail "invalid control bits";
        let address = Int64.add start (Int64.of_int offset) in
        let op = action previous ~start ~address ~finish flags in
        annotated := (flags land 3, op) :: !annotated;
        if flags land 1 <> 0 then add fallthrough address op;
        if flags land 2 <> 0 then add taken address op
      done;
      annotations := List.rev !annotated :: !annotations
    done;
    let table table what ~read ~equal ~to_string =
      let count = uleb () in
      for _ = 1 to count do
        let key : Hash.t = hash () in
        let value = read () in
        (match Hash.Tbl.find_opt table key with
        | Some old when not (equal old value) ->
          fail "%s collision %08lx: %s and %s" what
            (key :> int32)
            (to_string old) (to_string value)
        | Some _ | None -> ());
        Hash.Tbl.replace table key value
      done
    in
    table bodies "function body"
      ~read:(fun () -> Fdo_counter.Function_body_hash.of_int32 (int32 ()))
      ~equal:Fdo_counter.Function_body_hash.equal
      ~to_string:(fun (h : Fdo_counter.Function_body_hash.t) ->
        Printf.sprintf "%08lx" (h :> int32));
    table names "hash"
      ~read:(fun () ->
        let len = uleb () in
        need len;
        let value = String.sub data !pos len in
        pos := !pos + len;
        value)
      ~equal:String.equal
      ~to_string:(fun s -> s)
  done;
  Address_tbl.filter_map_inplace (fun _ ops -> Some (List.rev ops)) taken;
  Address_tbl.filter_map_inplace (fun _ ops -> Some (List.rev ops)) fallthrough;
  { taken;
    fallthrough = sorted fallthrough;
    functions = sorted functions;
    bodies;
    names;
    annotations = List.rev !annotations
  }

let print ppf t =
  let name (hash : Hash.t) =
    match Hash.Tbl.find_opt t.names hash with
    | Some name -> name
    | None -> Printf.sprintf "%08lx" (hash :> int32)
  in
  let stack stack = String.concat " < " (List.map name stack) in
  let mode = function 1 -> "fallthrough" | 2 -> "taken" | _ -> "both" in
  List.iter
    (fun annotations ->
      Format.fprintf ppf "function@.";
      List.iter
        (fun (bits, action) ->
          let kind, s =
            match action with
            | Call { stack; _ } -> "call", Some stack
            | Tailcall stack -> "tailcall", Some stack
            | Normal stack -> "normal", Some stack
            | Jump { stack; _ } -> "jump", Some stack
            | Reset -> "reset", None
          in
          match s with
          | None -> Format.fprintf ppf "  %s %s@." (mode bits) kind
          | Some s ->
            Format.fprintf ppf "  %s %s %s@." (mode bits) kind (stack s))
        annotations)
    t.annotations;
  Format.fprintf ppf "bodies:@.";
  Hash.Tbl.fold (fun hash body acc -> (name hash, body) :: acc) t.bodies []
  |> List.sort compare
  |> List.iter (fun (n, (body : Fdo_counter.Function_body_hash.t)) ->
      Format.fprintf ppf "  %s: %08lx@." n (body :> int32))

let lower_bound array address =
  let lo = ref 0 and hi = ref (Array.length array) in
  while !lo < !hi do
    let mid = (!lo + !hi) / 2 in
    if Int64.compare (fst array.(mid)) address < 0
    then lo := mid + 1
    else hi := mid
  done;
  !lo

module Trace_event = struct
  type t =
    | Branch of
        { source : int64;
          target : int64
        }
    | Count of Fdo_counter.hashed
    | Call of Fdo_counter.hashed
    | Tailcall of Fdo_counter.hashed
    | Return
    | Reset
    | Skip_call
    | Discard_context
end

(* Only cross-function context is execution-dependent. Real calls save their
   return PC; tail calls extend the context without adding a return address. *)
let apply callers ~count ~trace = function
  | Call { stack; return_address } ->
    count (stack @ List.concat_map fst !callers);
    trace (Trace_event.Call stack);
    callers := (stack, Some return_address) :: !callers
  | Tailcall stack ->
    count (stack @ List.concat_map fst !callers);
    trace (Trace_event.Tailcall stack);
    callers := (stack, None) :: !callers
  | Normal stack | Jump { stack; target = _ } ->
    count (stack @ List.concat_map fst !callers)
  | Reset ->
    trace Trace_event.Reset;
    callers := []

let return_to callers ~trace target =
  let rec unwind = function
    | [] -> None
    | (_, Some address) :: rest when Int64.equal address target -> Some rest
    | _ :: rest -> unwind rest
  in
  match unwind !callers with
  | Some rest ->
    trace Trace_event.Return;
    callers := rest
  | None -> ()

(* LBR supplies instruction boundaries. Recognize near RET, including legacy and
   REX prefixes, without disassembling the rest of the instruction set. *)
let is_return ~code_byte address =
  let byte offset =
    if offset >= 15
    then None
    else code_byte (Int64.add address (Int64.of_int offset))
  in
  let rec opcode offset =
    match byte offset with
    | Some 0xc3 -> true
    | Some 0xc2 ->
      Option.is_some (byte (offset + 1)) && Option.is_some (byte (offset + 2))
    | Some (0x26 | 0x2e | 0x36 | 0x3e | 0x64 | 0x65 | 0x66 | 0x67 | 0xf2 | 0xf3)
      ->
      opcode (offset + 1)
    | Some prefix when prefix >= 0x40 && prefix <= 0x4f -> opcode (offset + 1)
    | Some _ | None -> false
  in
  opcode 0

let process_sample ?(trace = ignore) t ~code_byte ~branches ~f =
  let state = ref [] in
  let count stack =
    trace (Trace_event.Count stack);
    f stack
  in
  let discard_context () =
    if not (List.is_empty !state) then trace Trace_event.Discard_context;
    state := []
  in
  let function_at address =
    let i = lower_bound t.functions address in
    let i =
      if
        i < Array.length t.functions
        && Int64.equal (fst t.functions.(i)) address
      then i
      else i - 1
    in
    if i >= 0 && Int64.compare address (snd t.functions.(i)) < 0
    then Some i
    else None
  in
  let range ~lo ~hi =
    match function_at lo with
    | Some i
      when Int64.compare lo hi <= 0
           && Int64.compare hi (snd t.functions.(i)) < 0 ->
      let first = lower_bound t.fallthrough lo in
      let last = lower_bound t.fallthrough hi in
      for i = first to last - 1 do
        List.iter (apply state ~count ~trace) (snd t.fallthrough.(i))
      done
    | Some _ | None -> ()
  in
  let previous_target = ref None in
  List.iter
    (fun (source, target) ->
      (* The oldest branch only sets up the state: overlapping samples, as the
         runtime's emulator writes them, then count everything once. *)
      let count =
        match !previous_target with
        | None -> ignore
        | Some lo ->
          range ~lo ~hi:source;
          count
      in
      trace (Trace_event.Branch { source; target });
      let source_available = Option.is_some (code_byte source) in
      let target_available = Option.is_some (code_byte target) in
      if not source_available then discard_context ();
      let actions =
        Option.value (Address_tbl.find_opt t.taken source) ~default:[]
      in
      let processed_call = ref false in
      List.iter
        (fun action ->
          match action with
          | Call _ | Tailcall _ ->
            if source_available && target_available
            then (
              processed_call := true;
              apply state ~count ~trace action)
            else trace Trace_event.Skip_call
          | Jump { target = jump_target; stack = _ } ->
            if Int64.equal jump_target target
            then apply state ~count ~trace action
          | Normal _ | Reset -> apply state ~count ~trace action)
        actions;
      if source_available && is_return ~code_byte source
      then return_to state ~trace target;
      (* An instrumented call can pass through an opaque stub in the same image.
         Other gaps discard caller context rather than invent missing
         ancestry. *)
      if
        (not target_available)
        || (not !processed_call)
           && Option.is_some (function_at source)
           && Option.is_none (function_at target)
      then discard_context ();
      previous_target := Some target)
    (List.rev branches)

(* The entry counter of the function containing an address, if any. *)
let function_entry t address =
  let i = lower_bound t.functions (Int64.succ address) - 1 in
  if i >= 0 && Int64.compare address (snd t.functions.(i)) < 0
  then
    let start = fst t.functions.(i) in
    let entry = function
      | Normal (hash :: _) -> Some hash
      | Normal [] | Call _ | Tailcall _ | Jump _ | Reset -> None
    in
    match
      Array.find_opt
        (fun (address, _) -> Int64.equal address start)
        t.fallthrough
    with
    | Some (_, actions) -> List.find_map entry actions
    | None -> None
  else None

let trace_printer t ppf =
  let name (hash : Hash.t) =
    match Hash.Tbl.find_opt t.names hash with
    | Some name -> name
    | None -> Printf.sprintf "%08lx" (hash :> int32)
  in
  let stack stack = String.concat " < " (List.map name stack) in
  let in_unannotated_code = ref false in
  fun (event : Trace_event.t) ->
    match event with
    | Branch { source; target } -> (
      match function_entry t source, function_entry t target with
      | None, None ->
        if not !in_unannotated_code
        then Format.fprintf ppf "branches in unannotated code@.";
        in_unannotated_code := true
      | source, target ->
        in_unannotated_code := false;
        let place = function Some hash -> name hash | None -> "?" in
        Format.fprintf ppf "branch %s -> %s@." (place source) (place target))
    | Count s -> Format.fprintf ppf "  count %s@." (stack s)
    | Call s -> Format.fprintf ppf "  call %s@." (stack s)
    | Tailcall s -> Format.fprintf ppf "  tailcall %s@." (stack s)
    | Return -> Format.fprintf ppf "  return@."
    | Reset -> Format.fprintf ppf "  reset@."
    | Skip_call -> Format.fprintf ppf "  skip call to unavailable code@."
    | Discard_context -> Format.fprintf ppf "  discard context@."

(* See backend/fdo_metadata_encode.ml for the wire format. *)

module Hash = Fdo_counter.Hash

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
  { bodies : Fdo_counter.Function_body_hash.t Hash.Tbl.t;
    names : string Hash.Tbl.t;
    annotations : (int * action) list list
        (** per function in metadata order: control bits and action *)
  }

let names t = t.names

let bodies t = t.bodies

let parse data =
  let names = Hash.Tbl.create 256 in
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
        annotated := (flags land 3, op) :: !annotated
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
  { bodies; names; annotations = List.rev !annotations }

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

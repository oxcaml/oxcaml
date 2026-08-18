(* Encodes module facts into the compact, table-driven block stored in index
   files, and validates such a block when decoding it. *)

module Facts = Module_implementation_facts

type t =
  { version : int;
    uids : Shape.Uid.t array;
    units : Compilation_unit.t array;
    files : string array;
    context_count : int;
    contexts : string;
    key_count : int;
    keys : string;
    checks : string;
    dependencies : string;
    equalities : string;
    omissions : string
  }

let version = 2

let max_context_depth = 1024

let max_context_expanded_size = 65_536

let clamp_to_array_cap semantic =
  let array_cap = Sys.max_array_length / 8 in
  if array_cap < semantic then array_cap else semantic

let max_table_entries = clamp_to_array_cap 500_000

let max_fact_rows = clamp_to_array_cap 262_144

let table_entry_cost_bytes = 512L

let fact_row_cost_bytes = 1024L

let max_decoded_budget_bytes = 268_435_456L

exception Malformed of string

let malformed fmt =
  Format.kasprintf (fun message -> raise (Malformed message)) fmt

module Decoded_budget = struct
  type t = { mutable estimated_bytes : int64 }

  let create () = { estimated_bytes = 0L }

  let saturating_add left right =
    let sum = Int64.add left right in
    if Int64.compare sum left < 0 then Int64.max_int else sum

  let charge t ~what ~count ~count_limit ~limit_name ~cost =
    if count < 0 then malformed "negative %s count %d" what count;
    if count > count_limit then
      malformed "%s count %d exceeds the %s limit %d" what count limit_name
        count_limit;
    t.estimated_bytes <-
      saturating_add t.estimated_bytes (Int64.mul (Int64.of_int count) cost);
    if Int64.compare t.estimated_bytes max_decoded_budget_bytes > 0 then
      malformed
        "%s entries push the estimated decoded size to %Ld bytes, over the \
         %Ld-byte budget"
        what t.estimated_bytes max_decoded_budget_bytes

  let charge_table_entries t ~what ~count =
    charge t ~what ~count ~count_limit:max_table_entries
      ~limit_name:"per-table entry" ~cost:table_entry_cost_bytes

  let charge_fact_rows t ~what ~count =
    charge t ~what ~count ~count_limit:max_fact_rows ~limit_name:"per-list row"
      ~cost:fact_row_cost_bytes
end

let zigzag n =
  Int64.logxor
    (Int64.shift_left (Int64.of_int n) 1)
    (Int64.shift_right (Int64.of_int n) 63)

let unzigzag encoded =
  Int64.logxor
    (Int64.shift_right_logical encoded 1)
    (Int64.neg (Int64.logand encoded 1L))

let add_int buffer n =
  let rec loop n =
    if Int64.equal (Int64.logand n (Int64.lognot 0x7fL)) 0L then
      Buffer.add_char buffer (Char.chr (Int64.to_int n))
    else begin
      Buffer.add_char buffer
        (Char.chr (0x80 lor Int64.to_int (Int64.logand n 0x7fL)));
      loop (Int64.shift_right_logical n 7)
    end
  in
  loop (zigzag n)

let add_uint buffer n =
  if n < 0 then malformed "negative unsigned value %d" n;
  let rec loop n =
    if n < 0x80 then Buffer.add_char buffer (Char.chr n)
    else begin
      Buffer.add_char buffer (Char.chr (0x80 lor (n land 0x7f)));
      loop (n lsr 7)
    end
  in
  loop n

let add_column buffer column value =
  add_int buffer (value - !column);
  column := value

type cursor = { payload : string; mutable position : int }

let read_uint64 cursor =
  let rec loop shift acc =
    if cursor.position >= String.length cursor.payload then
      malformed "truncated integer at byte %d" cursor.position
    else begin
      let byte = Char.code cursor.payload.[cursor.position] in
      cursor.position <- cursor.position + 1;
      if shift > 0 && byte = 0 then
        malformed "redundant integer encoding at byte %d" cursor.position;
      if shift = 63 && byte land 0x7e <> 0 then
        malformed "integer overflows 64 bits at byte %d" cursor.position;
      let acc =
        Int64.logor acc (Int64.shift_left (Int64.of_int (byte land 0x7f)) shift)
      in
      if byte land 0x80 = 0 then acc
      else if shift >= 63 then
        malformed "integer too long at byte %d" cursor.position
      else loop (shift + 7) acc
    end
  in
  loop 0 0L

let read_int cursor =
  let value = unzigzag (read_uint64 cursor) in
  if
    Int64.compare value (Int64.of_int min_int) < 0
    || Int64.compare value (Int64.of_int max_int) > 0
  then malformed "integer %Ld does not fit a native int" value;
  Int64.to_int value

let check_uint_in_native_range ~max_native_int ~position value =
  if Int64.compare value 0L < 0 || Int64.compare value max_native_int > 0 then
    malformed "integer overflows the native range at byte %d" position

let read_uint cursor =
  let value = read_uint64 cursor in
  check_uint_in_native_range ~max_native_int:(Int64.of_int max_int)
    ~position:cursor.position value;
  Int64.to_int value

module For_testing = struct
  let decode_canonical_uint ~max_native_int payload =
    let cursor = { payload; position = 0 } in
    match
      let value = read_uint64 cursor in
      check_uint_in_native_range ~max_native_int ~position:cursor.position value;
      if cursor.position <> String.length payload then
        malformed "%d trailing bytes after the integer"
          (String.length payload - cursor.position);
      value
    with
    | value -> Ok value
    | exception Malformed message -> Error message
end

let read_index cursor ~limit ~what =
  let index = read_uint cursor in
  if index >= limit then
    malformed "%s index %d out of range (table size %d)" what index limit;
  index

let read_column cursor column ~limit ~what =
  let value = !column + read_int cursor in
  if value < 0 || value >= limit then
    malformed "%s index %d out of range (table size %d)" what value limit;
  column := value;
  value

let finished cursor ~what =
  if cursor.position <> String.length cursor.payload then
    malformed "%s table has %d trailing bytes" what
      (String.length cursor.payload - cursor.position)

module String_tbl = Hashtbl.Make (struct
  type t = string

  let equal = String.equal

  let hash = Hashtbl.hash
end)

type context_entry =
  | E_def of int
  | E_app of int * int
  | E_proj of int * int
  | E_body of int
  | E_site of int * int * int

module Context_entry_tbl = Hashtbl.Make (struct
  type t = context_entry

  let equal left right =
    match (left, right) with
    | E_def a, E_def b -> Int.equal a b
    | E_app (a1, a2), E_app (b1, b2) -> Int.equal a1 b1 && Int.equal a2 b2
    | E_proj (a1, a2), E_proj (b1, b2) -> Int.equal a1 b1 && Int.equal a2 b2
    | E_body a, E_body b -> Int.equal a b
    | E_site (a1, a2, a3), E_site (b1, b2, b3) ->
      Int.equal a1 b1 && Int.equal a2 b2 && Int.equal a3 b3
    | (E_def _ | E_app _ | E_proj _ | E_body _ | E_site _), _ -> false

  let hash entry =
    let combine acc n = (acc * 486187739) + n + 1 in
    match entry with
    | E_def a -> combine 1 a
    | E_app (a, b) -> combine (combine 2 a) b
    | E_proj (a, b) -> combine (combine 3 a) b
    | E_body a -> combine 4 a
    | E_site (a, b, c) -> combine (combine (combine 5 a) b) c
end)

type key_entry = E_named of int * int | E_anon of int

module Key_entry_tbl = Hashtbl.Make (struct
  type t = key_entry

  let equal left right =
    match (left, right) with
    | E_named (a1, a2), E_named (b1, b2) -> Int.equal a1 b1 && Int.equal a2 b2
    | E_anon a, E_anon b -> Int.equal a b
    | (E_named _ | E_anon _), _ -> false

  let hash entry =
    let combine acc n = (acc * 486187739) + n + 1 in
    match entry with
    | E_named (a, b) -> combine (combine 1 a) b
    | E_anon a -> combine 2 a
end)

module Physical_context_tbl = Hashtbl.Make (struct
  type t = Facts.Context.t

  let equal = ( == )

  let hash = Hashtbl.hash
end)

type 'table interner = { table : 'table; mutable next : int; buffer : Buffer.t }

type context_metrics = { depth : int; expanded_size : int }

let check_context_metrics { depth; expanded_size } =
  if depth > max_context_depth then
    malformed "context depth %d exceeds limit %d" depth max_context_depth;
  if expanded_size > max_context_expanded_size then
    malformed "context expansion %d exceeds limit %d" expanded_size
      max_context_expanded_size

let leaf_metrics = { depth = 1; expanded_size = 1 }

let app_metrics functor_ argument =
  let metrics =
    { depth = 1 + max functor_.depth argument.depth;
      expanded_size = 1 + functor_.expanded_size + argument.expanded_size
    }
  in
  check_context_metrics metrics;
  metrics

let proj_metrics inner =
  let metrics =
    { depth = 1 + inner.depth; expanded_size = 1 + inner.expanded_size }
  in
  check_context_metrics metrics;
  metrics

let artifact_tag : Facts.Artifact.t -> int = function
  | Implementation -> 0
  | Interface -> 1

let artifact_of_tag = function
  | 0 -> Facts.Artifact.Implementation
  | 1 -> Facts.Artifact.Interface
  | tag -> malformed "unknown artifact tag %d" tag

let check_kind_tag : Facts.Check.Kind.t -> int = function
  | Ascription -> 0
  | Argument -> 1
  | Package -> 2
  | Interface -> 3

let check_kind_of_tag : int -> Facts.Check.Kind.t = function
  | 0 -> Ascription
  | 1 -> Argument
  | 2 -> Package
  | 3 -> Interface
  | tag -> malformed "unknown check kind tag %d" tag

let dependency_reason_tag : Facts.Dependency.Reason.t -> int = function
  | Definition -> 0
  | Alias -> 1
  | Include -> 2
  | With_constraint -> 3
  | Destructive_substitution -> 4
  | Module_type_of -> 5
  | Strengthening -> 6
  | Functor_type -> 7
  | Instance -> 8
  | Argument_member -> 9
  | Interface -> 10

let dependency_reason_of_tag : int -> Facts.Dependency.Reason.t = function
  | 0 -> Definition
  | 1 -> Alias
  | 2 -> Include
  | 3 -> With_constraint
  | 4 -> Destructive_substitution
  | 5 -> Module_type_of
  | 6 -> Strengthening
  | 7 -> Functor_type
  | 8 -> Instance
  | 9 -> Argument_member
  | 10 -> Interface
  | tag -> malformed "unknown dependency reason tag %d" tag

let omission_reason_tag : Facts.Omission.Reason.t -> int = function
  | Unresolved_module_type -> 0
  | Unresolved_module -> 1
  | Unsupported_path -> 2
  | Missing_parameter_expectation -> 3

let omission_reason_of_tag : int -> Facts.Omission.Reason.t = function
  | 0 -> Unresolved_module_type
  | 1 -> Unresolved_module
  | 2 -> Unsupported_path
  | 3 -> Missing_parameter_expectation
  | tag -> malformed "unknown omission reason tag %d" tag

let of_facts (facts : Facts.t) =
  let uids =
    { table = Shape.Uid.Tbl.create 64; next = 0; buffer = Buffer.create 0 }
  in
  let uids_rev = ref [] in
  let intern_uid uid =
    match Shape.Uid.Tbl.find_opt uids.table uid with
    | Some index -> index
    | None ->
      let index = uids.next in
      Shape.Uid.Tbl.add uids.table uid index;
      uids_rev := uid :: !uids_rev;
      uids.next <- index + 1;
      index
  in
  let units =
    { table = Compilation_unit.Tbl.create 4;
      next = 0;
      buffer = Buffer.create 0
    }
  in
  let units_rev = ref [] in
  let intern_unit unit_ =
    match Compilation_unit.Tbl.find_opt units.table unit_ with
    | Some index -> index
    | None ->
      let index = units.next in
      Compilation_unit.Tbl.add units.table unit_ index;
      units_rev := unit_ :: !units_rev;
      units.next <- index + 1;
      index
  in
  let files =
    { table = String_tbl.create 8; next = 0; buffer = Buffer.create 0 }
  in
  let files_rev = ref [] in
  let intern_file file =
    match String_tbl.find_opt files.table file with
    | Some index -> index
    | None ->
      let index = files.next in
      String_tbl.add files.table file index;
      files_rev := file :: !files_rev;
      files.next <- index + 1;
      index
  in
  let contexts =
    { table = Context_entry_tbl.create 64;
      next = 0;
      buffer = Buffer.create 256
    }
  in
  let context_uid_column = ref 0 in
  let context_context_column = ref 0 in
  let context_unit_column = ref 0 in
  let entry_metrics = Hashtbl.create 64 in
  let metrics_of_entry = function
    | E_def _ | E_body _ | E_site _ -> leaf_metrics
    | E_app (functor_, argument) ->
      app_metrics
        (Hashtbl.find entry_metrics functor_)
        (Hashtbl.find entry_metrics argument)
    | E_proj (context, _) -> proj_metrics (Hashtbl.find entry_metrics context)
  in
  let intern_context_entry entry =
    match Context_entry_tbl.find_opt contexts.table entry with
    | Some index -> index
    | None ->
      let metrics = metrics_of_entry entry in
      let index = contexts.next in
      Hashtbl.add entry_metrics index metrics;
      Context_entry_tbl.add contexts.table entry index;
      contexts.next <- index + 1;
      (match entry with
      | E_def uid ->
        add_uint contexts.buffer 0;
        add_column contexts.buffer context_uid_column uid
      | E_app (functor_, argument) ->
        add_uint contexts.buffer 1;
        add_column contexts.buffer context_context_column functor_;
        add_column contexts.buffer context_context_column argument
      | E_proj (context, uid) ->
        add_uint contexts.buffer 2;
        add_column contexts.buffer context_context_column context;
        add_column contexts.buffer context_uid_column uid
      | E_body uid ->
        add_uint contexts.buffer 3;
        add_column contexts.buffer context_uid_column uid
      | E_site (unit_, artifact, occurrence) ->
        add_uint contexts.buffer 4;
        add_column contexts.buffer context_unit_column unit_;
        add_uint contexts.buffer artifact;
        add_uint contexts.buffer occurrence);
      index
  in
  let context_memo : [ `Visiting | `Interned of int ] Physical_context_tbl.t =
    Physical_context_tbl.create 64
  in
  let interned_entry context =
    match Physical_context_tbl.find context_memo context with
    | `Interned index -> index
    | `Visiting -> malformed "cyclic context"
  in
  let intern_leaf (context : Facts.Context.t) =
    match context with
    | Def uid -> E_def (intern_uid uid)
    | Body uid -> E_body (intern_uid uid)
    | Site (unit_, artifact, occurrence) ->
      if occurrence < 0 then malformed "negative site occurrence %d" occurrence;
      E_site (intern_unit unit_, artifact_tag artifact, occurrence)
    | App _ | Proj _ -> assert false
  in
  let intern_context root =
    let work = ref [ `Visit (root, 1) ] in
    let pop () =
      match !work with
      | [] -> None
      | job :: rest ->
        work := rest;
        Some job
    in
    let rec loop () =
      match pop () with
      | None -> ()
      | Some (`Visit (context, depth)) ->
        if depth > max_context_depth then
          malformed "context depth %d exceeds limit %d" depth max_context_depth;
        (match Physical_context_tbl.find_opt context_memo context with
        | Some (`Interned _) -> ()
        | Some `Visiting -> malformed "cyclic context"
        | None -> (
          match (context : Facts.Context.t) with
          | Def _ | Body _ | Site _ ->
            Physical_context_tbl.add context_memo context
              (`Interned (intern_context_entry (intern_leaf context)))
          | App (functor_, argument) ->
            Physical_context_tbl.add context_memo context `Visiting;
            work :=
              `Visit (functor_, depth + 1)
              :: `Visit (argument, depth + 1)
              :: `Finish context :: !work
          | Proj (inner, _) ->
            Physical_context_tbl.add context_memo context `Visiting;
            work := `Visit (inner, depth + 1) :: `Finish context :: !work));
        loop ()
      | Some (`Finish context) ->
        (match (context : Facts.Context.t) with
        | App (functor_, argument) ->
          let functor_ = interned_entry functor_ in
          let argument = interned_entry argument in
          Physical_context_tbl.replace context_memo context
            (`Interned (intern_context_entry (E_app (functor_, argument))))
        | Proj (inner, uid) ->
          let inner = interned_entry inner in
          Physical_context_tbl.replace context_memo context
            (`Interned (intern_context_entry (E_proj (inner, intern_uid uid))))
        | Def _ | Body _ | Site _ -> assert false);
        loop ()
    in
    loop ();
    interned_entry root
  in
  let keys =
    { table = Key_entry_tbl.create 64; next = 0; buffer = Buffer.create 128 }
  in
  let key_context_column = ref 0 in
  let key_uid_column = ref 0 in
  let intern_key_entry entry =
    match Key_entry_tbl.find_opt keys.table entry with
    | Some index -> index
    | None ->
      let index = keys.next in
      Key_entry_tbl.add keys.table entry index;
      keys.next <- index + 1;
      (match entry with
      | E_named (context, uid) ->
        add_uint keys.buffer 0;
        add_column keys.buffer key_context_column context;
        add_column keys.buffer key_uid_column uid
      | E_anon uid ->
        add_uint keys.buffer 1;
        add_column keys.buffer key_uid_column uid);
      index
  in
  let intern_key : Facts.Key.t -> int = function
    | Named (context, uid) ->
      let context = intern_context context in
      intern_key_entry (E_named (context, intern_uid uid))
    | Anon uid -> intern_key_entry (E_anon (intern_uid uid))
  in
  let add_location buffer (location : Location.t) =
    add_uint buffer (if location.loc_ghost then 1 else 0);
    let start = location.loc_start in
    let finish = location.loc_end in
    add_uint buffer (intern_file start.pos_fname);
    add_int buffer start.pos_lnum;
    add_int buffer start.pos_bol;
    add_int buffer (start.pos_cnum - start.pos_bol);
    add_uint buffer (intern_file finish.pos_fname);
    add_int buffer (finish.pos_lnum - start.pos_lnum);
    add_int buffer (finish.pos_bol - start.pos_bol);
    add_int buffer (finish.pos_cnum - finish.pos_bol)
  in
  let checks_buffer = Buffer.create 256 in
  add_uint checks_buffer (List.length facts.checks);
  let check_uid_column = ref 0 in
  let check_unit_column = ref 0 in
  let check_expectation_column = ref 0 in
  List.iter
    (fun ({ implementation; expectation; kind; site } : Facts.Check.t) ->
      (match implementation with
      | Facts.Node.Uid uid ->
        add_uint checks_buffer 0;
        add_column checks_buffer check_uid_column (intern_uid uid)
      | Facts.Node.Location (unit_, location) ->
        add_uint checks_buffer 1;
        add_column checks_buffer check_unit_column (intern_unit unit_);
        add_location checks_buffer location);
      add_column checks_buffer check_expectation_column (intern_key expectation);
      add_uint checks_buffer (check_kind_tag kind);
      add_location checks_buffer site)
    facts.checks;
  let dependencies_buffer = Buffer.create 256 in
  add_uint dependencies_buffer (List.length facts.dependencies);
  let derived_column = ref 0 in
  let source_column = ref 0 in
  List.iter
    (fun ({ derived; source; reason } : Facts.Dependency.t) ->
      add_column dependencies_buffer derived_column (intern_key derived);
      add_column dependencies_buffer source_column (intern_key source);
      add_uint dependencies_buffer (dependency_reason_tag reason))
    facts.dependencies;
  let equalities_buffer = Buffer.create 64 in
  add_uint equalities_buffer (List.length facts.equalities);
  let left_column = ref 0 in
  let right_column = ref 0 in
  List.iter
    (fun ({ left; right } : Facts.Context_equality.t) ->
      add_column equalities_buffer left_column (intern_context left);
      add_column equalities_buffer right_column (intern_context right))
    facts.equalities;
  let omissions_buffer = Buffer.create 64 in
  add_uint omissions_buffer (List.length facts.omissions);
  let omission_key_column = ref 0 in
  let omission_uid_column = ref 0 in
  List.iter
    (fun ({ affected; source; reason } : Facts.Omission.t) ->
      (match affected with
      | None -> add_uint omissions_buffer 0
      | Some key ->
        add_uint omissions_buffer 1;
        add_column omissions_buffer omission_key_column (intern_key key));
      (match source with
      | None -> add_uint omissions_buffer 0
      | Some uid ->
        add_uint omissions_buffer 1;
        add_column omissions_buffer omission_uid_column (intern_uid uid));
      add_uint omissions_buffer (omission_reason_tag reason))
    facts.omissions;
  { version;
    uids = Array.of_list (List.rev !uids_rev);
    units = Array.of_list (List.rev !units_rev);
    files = Array.of_list (List.rev !files_rev);
    context_count = contexts.next;
    contexts = Buffer.contents contexts.buffer;
    key_count = keys.next;
    keys = Buffer.contents keys.buffer;
    checks = Buffer.contents checks_buffer;
    dependencies = Buffer.contents dependencies_buffer;
    equalities = Buffer.contents equalities_buffer;
    omissions = Buffer.contents omissions_buffer
  }

let empty = of_facts Facts.empty

let no_entries packed = String.equal packed "\x00"

let is_empty t =
  Array.length t.uids = 0
  && Array.length t.units = 0
  && Array.length t.files = 0
  && t.context_count = 0 && t.key_count = 0 && no_entries t.checks
  && no_entries t.dependencies && no_entries t.equalities
  && no_entries t.omissions

let validate_unique_uids uids =
  let seen = Shape.Uid.Tbl.create (Array.length uids) in
  Array.iter
    (fun uid ->
      if Shape.Uid.Tbl.mem seen uid then
        malformed "duplicate uid table entry %s"
          (Format.asprintf "%a" Shape.Uid.print uid);
      Shape.Uid.Tbl.add seen uid ())
    uids

let validate_unique_units units =
  let seen = Compilation_unit.Tbl.create (Array.length units) in
  Array.iter
    (fun unit_ ->
      if Compilation_unit.Tbl.mem seen unit_ then
        malformed "duplicate compilation unit table entry %s"
          (Compilation_unit.full_path_as_string unit_);
      Compilation_unit.Tbl.add seen unit_ ())
    units

let validate_unique_files files =
  let seen = String_tbl.create (Array.length files) in
  Array.iter
    (fun file ->
      if String_tbl.mem seen file then
        malformed "duplicate file table entry %S" file;
      String_tbl.add seen file ())
    files

let decode_contexts t budget =
  let cursor = { payload = t.contexts; position = 0 } in
  let count = t.context_count in
  if count < 0 then malformed "negative context count %d" count;
  Decoded_budget.charge_table_entries budget ~what:"context" ~count;
  if count > String.length t.contexts then
    malformed "context count %d exceeds table size %d" count
      (String.length t.contexts);
  let contexts =
    Array.make count (Facts.Context.Def Shape.Uid.internal_not_actually_unique)
  in
  let metrics = Array.make count leaf_metrics in
  let seen = Context_entry_tbl.create count in
  let uid_column = ref 0 in
  let context_column = ref 0 in
  let unit_column = ref 0 in
  for index = 0 to count - 1 do
    let entry =
      match read_uint cursor with
      | 0 ->
        E_def
          (read_column cursor uid_column ~limit:(Array.length t.uids)
             ~what:"uid")
      | 1 ->
        let functor_ =
          read_column cursor context_column ~limit:index ~what:"context"
        in
        let argument =
          read_column cursor context_column ~limit:index ~what:"context"
        in
        E_app (functor_, argument)
      | 2 ->
        let context =
          read_column cursor context_column ~limit:index ~what:"context"
        in
        let uid =
          read_column cursor uid_column ~limit:(Array.length t.uids) ~what:"uid"
        in
        E_proj (context, uid)
      | 3 ->
        E_body
          (read_column cursor uid_column ~limit:(Array.length t.uids)
             ~what:"uid")
      | 4 ->
        let unit_ =
          read_column cursor unit_column ~limit:(Array.length t.units)
            ~what:"unit"
        in
        let artifact = read_uint cursor in
        let occurrence = read_uint cursor in
        E_site (unit_, artifact, occurrence)
      | tag -> malformed "unknown context tag %d" tag
    in
    if Context_entry_tbl.mem seen entry then
      malformed "duplicate context table entry %d" index;
    Context_entry_tbl.add seen entry ();
    metrics.(index) <-
      (match entry with
      | E_def _ | E_body _ | E_site _ -> leaf_metrics
      | E_app (functor_, argument) ->
        app_metrics metrics.(functor_) metrics.(argument)
      | E_proj (context, _) -> proj_metrics metrics.(context));
    contexts.(index) <-
      (match entry with
      | E_def uid -> Def t.uids.(uid)
      | E_app (functor_, argument) ->
        App (contexts.(functor_), contexts.(argument))
      | E_proj (context, uid) -> Proj (contexts.(context), t.uids.(uid))
      | E_body uid -> Body t.uids.(uid)
      | E_site (unit_, artifact, occurrence) ->
        Site (t.units.(unit_), artifact_of_tag artifact, occurrence))
  done;
  finished cursor ~what:"context";
  contexts

let decode_keys t contexts budget =
  let cursor = { payload = t.keys; position = 0 } in
  let count = t.key_count in
  if count < 0 then malformed "negative key count %d" count;
  Decoded_budget.charge_table_entries budget ~what:"key" ~count;
  if count > String.length t.keys then
    malformed "key count %d exceeds table size %d" count (String.length t.keys);
  let keys =
    Array.make count (Facts.Key.Anon Shape.Uid.internal_not_actually_unique)
  in
  let seen = Key_entry_tbl.create count in
  let context_column = ref 0 in
  let uid_column = ref 0 in
  for index = 0 to count - 1 do
    let entry =
      match read_uint cursor with
      | 0 ->
        let context =
          read_column cursor context_column ~limit:(Array.length contexts)
            ~what:"context"
        in
        let uid =
          read_column cursor uid_column ~limit:(Array.length t.uids) ~what:"uid"
        in
        E_named (context, uid)
      | 1 ->
        E_anon
          (read_column cursor uid_column ~limit:(Array.length t.uids)
             ~what:"uid")
      | tag -> malformed "unknown key tag %d" tag
    in
    if Key_entry_tbl.mem seen entry then
      malformed "duplicate key table entry %d" index;
    Key_entry_tbl.add seen entry ();
    keys.(index) <-
      (match entry with
      | E_named (context, uid) -> Named (contexts.(context), t.uids.(uid))
      | E_anon uid -> Anon t.uids.(uid))
  done;
  finished cursor ~what:"key";
  keys

let decode_list cursor budget ~what ~element =
  let count = read_uint cursor in
  Decoded_budget.charge_fact_rows budget ~what ~count;
  let remaining_bytes = String.length cursor.payload - cursor.position in
  if count > remaining_bytes then
    malformed "%s count %d exceeds remaining %d payload bytes" what count
      remaining_bytes;
  let rec loop remaining acc =
    if remaining = 0 then List.rev acc
    else loop (remaining - 1) (element () :: acc)
  in
  let elements = loop count [] in
  finished cursor ~what;
  elements

let to_facts_exn t =
  if t.version <> version then
    malformed "unsupported compact facts version %d (expected %d)" t.version
      version;
  let budget = Decoded_budget.create () in
  Decoded_budget.charge_table_entries budget ~what:"uid"
    ~count:(Array.length t.uids);
  Decoded_budget.charge_table_entries budget ~what:"compilation unit"
    ~count:(Array.length t.units);
  Decoded_budget.charge_table_entries budget ~what:"file"
    ~count:(Array.length t.files);
  validate_unique_uids t.uids;
  validate_unique_units t.units;
  validate_unique_files t.files;
  let contexts = decode_contexts t budget in
  let keys = decode_keys t contexts budget in
  let read_file cursor =
    t.files.(read_index cursor ~limit:(Array.length t.files) ~what:"file")
  in
  let read_location cursor : Location.t =
    let loc_ghost =
      match read_uint cursor with
      | 0 -> false
      | 1 -> true
      | flag -> malformed "unknown ghost flag %d" flag
    in
    let start_fname = read_file cursor in
    let start_lnum = read_int cursor in
    let start_bol = read_int cursor in
    let start_cnum = start_bol + read_int cursor in
    let end_fname = read_file cursor in
    let end_lnum = start_lnum + read_int cursor in
    let end_bol = start_bol + read_int cursor in
    let end_cnum = end_bol + read_int cursor in
    { loc_start =
        { pos_fname = start_fname;
          pos_lnum = start_lnum;
          pos_bol = start_bol;
          pos_cnum = start_cnum
        };
      loc_end =
        { pos_fname = end_fname;
          pos_lnum = end_lnum;
          pos_bol = end_bol;
          pos_cnum = end_cnum
        };
      loc_ghost
    }
  in
  let checks_cursor = { payload = t.checks; position = 0 } in
  let check_uid_column = ref 0 in
  let check_unit_column = ref 0 in
  let check_expectation_column = ref 0 in
  let checks =
    decode_list checks_cursor budget ~what:"check"
      ~element:(fun () : Facts.Check.t ->
        let implementation =
          match read_uint checks_cursor with
          | 0 ->
            Facts.Node.Uid
              t.uids.(read_column checks_cursor check_uid_column
                        ~limit:(Array.length t.uids) ~what:"uid")
          | 1 ->
            let unit_ =
              t.units.(read_column checks_cursor check_unit_column
                         ~limit:(Array.length t.units) ~what:"unit")
            in
            let location = read_location checks_cursor in
            Facts.Node.Location (unit_, location)
          | tag -> malformed "unknown node tag %d" tag
        in
        let expectation =
          keys.(read_column checks_cursor check_expectation_column
                  ~limit:(Array.length keys) ~what:"key")
        in
        let kind = check_kind_of_tag (read_uint checks_cursor) in
        let site = read_location checks_cursor in
        { implementation; expectation; kind; site })
  in
  let dependencies_cursor = { payload = t.dependencies; position = 0 } in
  let derived_column = ref 0 in
  let source_column = ref 0 in
  let dependencies =
    decode_list dependencies_cursor budget ~what:"dependency"
      ~element:(fun () : Facts.Dependency.t ->
        let derived =
          keys.(read_column dependencies_cursor derived_column
                  ~limit:(Array.length keys) ~what:"key")
        in
        let source =
          keys.(read_column dependencies_cursor source_column
                  ~limit:(Array.length keys) ~what:"key")
        in
        let reason = dependency_reason_of_tag (read_uint dependencies_cursor) in
        { derived; source; reason })
  in
  let equalities_cursor = { payload = t.equalities; position = 0 } in
  let left_column = ref 0 in
  let right_column = ref 0 in
  let equalities =
    decode_list equalities_cursor budget ~what:"context equality"
      ~element:(fun () : Facts.Context_equality.t ->
        let left =
          contexts.(read_column equalities_cursor left_column
                      ~limit:(Array.length contexts) ~what:"context")
        in
        let right =
          contexts.(read_column equalities_cursor right_column
                      ~limit:(Array.length contexts) ~what:"context")
        in
        { left; right })
  in
  let omissions_cursor = { payload = t.omissions; position = 0 } in
  let omission_key_column = ref 0 in
  let omission_uid_column = ref 0 in
  let omissions =
    decode_list omissions_cursor budget ~what:"omission"
      ~element:(fun () : Facts.Omission.t ->
        let affected =
          match read_uint omissions_cursor with
          | 0 -> None
          | 1 ->
            Some
              keys.(read_column omissions_cursor omission_key_column
                      ~limit:(Array.length keys) ~what:"key")
          | tag -> malformed "unknown option tag %d" tag
        in
        let source =
          match read_uint omissions_cursor with
          | 0 -> None
          | 1 ->
            Some
              t.uids.(read_column omissions_cursor omission_uid_column
                        ~limit:(Array.length t.uids) ~what:"uid")
          | tag -> malformed "unknown option tag %d" tag
        in
        let reason = omission_reason_of_tag (read_uint omissions_cursor) in
        { affected; source; reason })
  in
  Facts.normalize { checks; dependencies; equalities; omissions }

let to_facts t =
  match to_facts_exn t with
  | facts -> Ok facts
  | exception Malformed message -> Error message

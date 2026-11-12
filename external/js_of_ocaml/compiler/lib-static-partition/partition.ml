open! Js_of_ocaml_compiler.Stdlib

module Unit = struct
  type t =
    { name : string
    ; provides : string list
    ; requires : string list
    ; imports : (string * string option) list
    ; force_link : bool
    }
end

module Report = struct
  type t =
    { eager : string list
    ; lazy_ : string list
    ; shared : string list
    ; forced_eager : string list
    ; unused : string list
    ; main_exports : string list
    }

  let to_json { eager; lazy_; shared; forced_eager; unused; main_exports } =
    let names l = `List (List.map l ~f:(fun name -> `String name)) in
    `Assoc
      [ "eager", names eager
      ; "lazy", names lazy_
      ; "shared", names shared
      ; "forced_eager", names forced_eager
      ; "unused", names unused
      ; "main_exports", names main_exports
      ]
end

type t =
  { eager : Unit.t list
  ; lazy_units : Unit.t list
  ; lazy_roots : (string * Unit.t list) list
  ; report : Report.t
  }

let eager t = t.eager

let lazy_units t = t.lazy_units

let lazy_roots t = t.lazy_roots

let report t = t.report

let provided_names (unit_ : Unit.t) = StringSet.of_list (unit_.name :: unit_.provides)

let required_names (unit_ : Unit.t) =
  StringSet.diff (StringSet.of_list unit_.requires) (provided_names unit_)

let failwithf fmt = Printf.ksprintf failwith fmt

let path_to_string path = String.concat ~sep:" -> " path

let create_many
    ?(check_deferred_handles = [])
    units
    ~main_unit
    ~lazy_units
    ~require_deferred_units =
  let units = Array.of_list units in
  let providers = String.Hashtbl.create 64 in
  let imports = String.Hashtbl.create 64 in
  Array.iteri units ~f:(fun index (unit_ : Unit.t) ->
      StringSet.iter
        (fun provided ->
          match String.Hashtbl.find_opt providers provided with
          | None -> String.Hashtbl.replace providers provided index
          | Some previous ->
              failwithf
                "Duplicate compilation-unit provider %s (provided by %s and %s)"
                provided
                units.(previous).Unit.name
                unit_.name)
        (provided_names unit_);
      List.iter unit_.imports ~f:(fun (name, crc) ->
          Option.iter crc ~f:(fun crc ->
              match String.Hashtbl.find_opt imports name with
              | None -> String.Hashtbl.replace imports name (crc, unit_.name)
              | Some (previous_crc, previous_unit) ->
                  if not (String.equal previous_crc crc)
                  then
                    failwithf
                      "Inconsistent interface imports of %s (by %s and %s)"
                      name
                      previous_unit
                      unit_.name)));
  let provider_exn required ~requester =
    match String.Hashtbl.find_opt providers required with
    | Some index -> index
    | None ->
        failwithf
          "Unresolved compilation-unit dependency %s (required by %s)"
          required
          requester
  in
  let main_index = provider_exn main_unit ~requester:"main root" in
  if List.is_empty lazy_units then failwith "At least one lazy root is required";
  let lazy_indices =
    List.map lazy_units ~f:(fun name -> name, provider_exn name ~requester:"lazy root")
  in
  let root_owners = Int.Hashtbl.create 16 in
  List.iter lazy_indices ~f:(fun (name, index) ->
      match Int.Hashtbl.find_opt root_owners index with
      | None -> Int.Hashtbl.replace root_owners index name
      | Some previous ->
          failwithf "Lazy roots %s and %s share a compilation-unit provider" previous name);
  let closure roots =
    let visiting = Int.Hashtbl.create 64 in
    let visited = ref IntSet.empty in
    let rec visit index path =
      if Int.Hashtbl.mem visiting index
      then
        failwithf
          "Cyclic compilation-unit dependencies: %s"
          (path_to_string (List.rev (units.(index).Unit.name :: path)));
      if not (IntSet.mem index !visited)
      then (
        Int.Hashtbl.replace visiting index ();
        let unit_ = units.(index) in
        StringSet.iter
          (fun required ->
            visit (provider_exn required ~requester:unit_.name) (unit_.name :: path))
          (required_names unit_);
        Int.Hashtbl.remove visiting index;
        visited := IntSet.add index !visited)
    in
    List.iter roots ~f:(fun index -> visit index []);
    !visited
  in
  let forced =
    List.filter
      (List.init ~len:(Array.length units) ~f:Fun.id)
      ~f:(fun index -> units.(index).Unit.force_link)
  in
  let eager = closure (main_index :: forced) in
  List.iter lazy_indices ~f:(fun (lazy_unit, lazy_index) ->
      if IntSet.mem lazy_index eager
      then
        failwithf
          "Lazy root %s is required by the eager partition (including force-linked roots)"
          lazy_unit);
  let closures =
    List.map lazy_indices ~f:(fun (name, index) -> name, closure [ index ])
  in
  (* The dependency path from a root to [target], if any, memoized per target. *)
  let dependency_paths target =
    let paths = Int.Hashtbl.create 64 in
    let rec visit index =
      match Int.Hashtbl.find_opt paths index with
      | Some path -> path
      | None ->
          let path =
            if index = target
            then Some [ units.(index).Unit.name ]
            else
              List.find_map
                (StringSet.elements (required_names units.(index)))
                ~f:(fun required ->
                  visit (provider_exn required ~requester:units.(index).name)
                  |> Option.map ~f:(fun path -> units.(index).name :: path))
          in
          Int.Hashtbl.replace paths index path;
          path
    in
    visit
  in
  List.iter check_deferred_handles ~f:(fun (handle, implementation) ->
      let handle_index = provider_exn handle ~requester:"deferred handle" in
      let implementation_index =
        provider_exn implementation ~requester:"deferred implementation"
      in
      if IntSet.mem implementation_index (closure [ handle_index ])
      then
        failwithf
          "Deferred handle %s has a runtime dependency on its implementation %s: %s"
          handle
          implementation
          (path_to_string
             (Option.value
                ~default:[]
                (dependency_paths implementation_index handle_index))));
  let declared =
    List.sort_uniq ~cmp:String.compare (lazy_units @ require_deferred_units)
  in
  List.iter declared ~f:(fun name ->
      let index = provider_exn name ~requester:"imported implementation" in
      let owners =
        List.filter closures ~f:(fun (_, indices) -> IntSet.mem index indices)
      in
      if IntSet.mem index eager || List.length owners <> 1
      then
        let dependency_path = dependency_paths index in
        let paths =
          List.filter_map
            ((("eager", main_index) :: List.map forced ~f:(fun index -> "forced", index))
            @ lazy_indices)
            ~f:(fun (root, root_index) ->
              Option.map (dependency_path root_index) ~f:(fun path ->
                  Printf.sprintf "%s: %s" root (path_to_string path)))
        in
        failwithf
          "Imported implementation %s must belong to exactly one lazy root; move shared \
           values and types into an ordinary module and use the generated handle (%s)"
          name
          (String.concat ~sep:"; " paths));
  let shared_lazy =
    IntSet.filter
      (fun index ->
        List.length
          (List.filter closures ~f:(fun (_, indices) -> IntSet.mem index indices))
        > 1)
      (IntSet.of_list (List.init ~len:(Array.length units) ~f:Fun.id))
  in
  let eager = IntSet.union eager (closure (IntSet.elements shared_lazy)) in
  let lazy_closure =
    List.fold_left closures ~init:IntSet.empty ~f:(fun acc (_, indices) ->
        IntSet.union acc indices)
  in
  let lazy_only = IntSet.diff lazy_closure eager in
  let selected = IntSet.union eager lazy_only in
  IntSet.iter
    (fun index ->
      let unit_ = units.(index) in
      StringSet.iter
        (fun required ->
          let dependency = provider_exn required ~requester:unit_.name in
          if dependency >= index
          then
            failwithf
              "Inputs are not in initialization order: %s requires %s"
              unit_.name
              required)
        (required_names unit_))
    selected;
  let select indices =
    List.filteri (Array.to_list units) ~f:(fun index _ -> IntSet.mem index indices)
  in
  let names units = List.map units ~f:(fun (unit_ : Unit.t) -> unit_.name) in
  let eager_units = select eager in
  let lazy_units = select lazy_only in
  let union units ~f =
    List.fold_left units ~init:StringSet.empty ~f:(fun acc unit_ ->
        StringSet.union acc (f unit_))
  in
  let lazy_external =
    StringSet.diff
      (union lazy_units ~f:required_names)
      (union lazy_units ~f:provided_names)
  in
  let main_exports =
    StringSet.inter lazy_external (union eager_units ~f:provided_names)
    |> StringSet.elements
  in
  { eager = eager_units
  ; lazy_units
  ; lazy_roots =
      List.map closures ~f:(fun (name, indices) ->
          name, select (IntSet.diff indices eager))
  ; report =
      { eager = names eager_units
      ; lazy_ = names lazy_units
      ; shared = names (select (IntSet.inter eager lazy_closure))
      ; forced_eager = List.map forced ~f:(fun index -> units.(index).Unit.name)
      ; unused =
          names
            (List.filteri (Array.to_list units) ~f:(fun index _ ->
                 not (IntSet.mem index selected)))
      ; main_exports
      }
  }

let create units ~main_unit ~lazy_unit =
  create_many units ~main_unit ~lazy_units:[ lazy_unit ] ~require_deferred_units:[]

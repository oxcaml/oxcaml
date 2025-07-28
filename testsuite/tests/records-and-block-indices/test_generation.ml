(* This file is used in [do_test_generation.ml]. *)

open Stdlib_upstream_compatible
open Stdlib_stable
module String = StringLabels
module List = ListLabels
open Metaprogramming

let sprintf = Printf.sprintf

let failwithf fmt = Printf.ksprintf failwith fmt

let interesting_type_trees : Type_structure.t Tree.t list =
  (* There are many possible type trees, exponential in the size of the tree and
     the number of types we consider.

     We balance these factors by combining e.g. a collection of trees with
     many/complex shapes but few types and a collection of trees with a few
     shapes but many types. And so on. *)
  let patient =
    (* Enable extra tests that take too long to compile (particularly on
       native), and thus are run locally only *)
    false
  in
  let open Type_structure in
  List.concat_map
    [ [Int; Float];
      [Int; Int32_u];
      [Int; Int64x2_u];
      [Float; Float_u];
      [Unit_u; String]
    ]
    ~f:(fun leaves ->
      List.concat_map
        (Tree.enumerate_shapes' ~max_leaves_and_singleton_branches:3
        @
        if patient
        then
          [ Branch [Leaf (); Branch [Leaf (); Branch [Leaf ()]]];
            Branch [Leaf (); Branch [Branch [Leaf (); Leaf ()]]];
            Branch [Branch [Branch [Leaf (); Leaf ()]; Leaf ()]];
            Branch [Branch [Leaf (); Branch [Leaf (); Leaf ()]]]
          ]
        else []
        )
        ~f:(fun shape -> Tree.enumerate ~shape ~leaves)
    )
  @ List.concat_map
      (Tree.enumerate_shapes' ~max_leaves_and_singleton_branches:2)
      ~f:(fun shape ->
        Tree.enumerate ~shape
          ~leaves:
            ([ Int;
               Int64;
               Int32_u;
               Float;
               Int64_u;
               Nativeint_u;
               Unit_u;
               Variant [[Unit_u]]
             ]
            @
            if patient
            then
              [ Tuple ([Unit_u; Unit_u], Unboxed);
                Tuple ([Unit_u; Int], Unboxed);
                Tuple ([Unit_u; Int64_u], Unboxed);
                Tuple ([Unit_u; String], Unboxed);
                Variant
                  [ [Unit_u];
                    [Unit_u; Unit_u];
                    [Tuple ([Unit_u; Unit_u], Unboxed)]
                  ]
              ]
            else []
            )
    )
  @ (* Some particular interesting trees *)
  [ Branch
      (* Mixed then all flat *)
      [Branch [Leaf Int64; Leaf Int64_u]; Branch [Leaf Int64_u; Leaf Float_u]];
    Branch
      (* Mixed then all values *)
      [Branch [Leaf Int64_u; Leaf Int64]; Branch [Leaf Int64; Leaf Int64]];
    Branch
      (* All values then mixed *)
      [Branch [Leaf Int64; Leaf String]; Branch [Leaf Int64_u; Leaf String]];
    Branch
      (* All flats then mixed *)
      [Branch [Leaf Float32_u; Leaf Int64_u]; Branch [Leaf String; Leaf Int64_u]];
    Branch
      (* Mixed then mixed *)
      [Branch [Leaf Int64_u; Leaf Int64]; Branch [Leaf Float32_u; Leaf Float]];
    Branch
      (* An int64x2 that would be reordered to the gap of a "sibling" record *)
      [Branch [Leaf Int64x2_u; Leaf String]; Branch [Leaf Int64; Leaf Float_u]];
    Branch
      (* An int64x2 that would be reordered to the gap of an inner record *)
      [Leaf Int64x2_u; Branch [Leaf String; Leaf Float_u]]
  ]
  |> List.sort_uniq ~cmp:(Tree.compare Type_structure.compare)

let preamble =
  {|open Stdlib_upstream_compatible
open Stdlib_stable
open Stdlib_beta

external[@layout_poly] makearray_dynamic_local :
  ('a : any mod separable) . int -> 'a -> 'a array @ local =
  "%makearray_dynamic"

external[@layout_poly] makearray_dynamic :
  ('a : any mod separable) . int -> 'a -> 'a array =
  "%makearray_dynamic"

external[@layout_poly] get :
  ('a : any mod separable) . ('a array[@local_opt]) -> (int[@local_opt]) -> 'a =
  "%array_safe_get"

external[@layout_poly] set :
  ('a : any mod separable) . ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit =
  "%array_safe_set"

let failwithf fmt = Printf.ksprintf failwith fmt

(* Redefine iter to infer locality *)
let rec iter ~f = function
    [] -> ()
  | a::l -> f a; iter ~f l

module Int_set = Set.Make(Int)

let tests_run = ref Int_set.empty

let mark_test_run test_id =
  tests_run := Int_set.add test_id !tests_run

(* Various interesting values *)

let sizes = [ 0; 1; 2; 30 ]

type packed = P : 'a -> packed
let ref_to_force_heap_allocation : packed ref = ref (P 0)
|}

let indent = ref 0

let with_indent f =
  incr indent;
  f ();
  decr indent

let line fmt =
  Printf.ksprintf
    (fun s ->
      let indent = Seq.init (!indent * 2) (fun _ -> ' ') |> String.of_seq in
      print_endline (indent ^ s);
      flush stdout
    )
    fmt

let print_in_test s =
  line {|let () = Printf.printf "%s%%!\n";;|} (String.escaped s)

let seq_print_in_test s = line {|print_endline "%s%!";|} (String.escaped s)

let makearray_dynamic_fn ~local =
  let local_s = if local then "_local" else "" in
  "makearray_dynamic" ^ local_s

type debug_expr =
  { expr : string;
    format_s : string
  }

let concat_with_leading_spaces l =
  List.map l ~f:(fun s -> " " ^ s) |> String.concat ~sep:""

let combine_debug_exprs (l : debug_expr list) : debug_expr =
  let debug_expr_to_tuple { expr; format_s } = expr, format_s in
  let exprs, format_ss = List.split (List.rev_map ~f:debug_expr_to_tuple l) in
  let expr = concat_with_leading_spaces exprs in
  let format_s = concat_with_leading_spaces format_ss in
  { expr; format_s }

let seq_print_debug_exprs ~debug_exprs =
  let { expr; format_s } = combine_debug_exprs debug_exprs in
  line {|Printf.printf "%s: %s\n%%!"%s;|} expr format_s expr

let test_id = ref 0

let seq_assert ~debug_exprs s =
  incr test_id;
  let { expr; format_s } = combine_debug_exprs debug_exprs in
  line "mark_test_run %d;" !test_id;
  line "let test = %s in" s;
  line {|if not test then failwithf "test %d failed%s"%s;|} !test_id format_s
    expr

let for_ var ~from ~to_ ~debug_exprs f =
  line "for %s = %s to %s do" var from to_;
  with_indent (fun () ->
      let debug_exprs = { expr = var; format_s = "%d" } :: debug_exprs in
      f ~debug_exprs
  );
  line "done;"

(* Iterate through a list of ints *)
let iter l var ~debug_exprs f =
  line "iter (%s) ~f:(fun %s ->" l var;
  with_indent (fun () ->
      let debug_exprs = { expr = var; format_s = "%d" } :: debug_exprs in
      f ~debug_exprs
  );
  line ") [@nontail];"

let section s =
  let s_as_stars = String.init (String.length s) ~f:(fun _ -> '*') in
  line "(**%s**)" s_as_stars;
  line "(* %s *)" s;
  line "(**%s**)" s_as_stars

let type_section (ty : Type.t) =
  let header =
    match ty with
    | Record _ ->
      (* show the structure of nominal types to reduce definition-chasing *)
      sprintf "%s = %s" (Type.code ty)
        (Type_structure.to_string (Type.structure ty))
    | _ -> Type.code ty
  in
  section ("  " ^ header ^ "  ")

let test_record_size ty ~bytecode =
  type_section ty;
  line "let r = %s in" (Type.value_code ty 0);
  let expected_size = Type_structure.size (Type.structure ty) ~bytecode in
  seq_assert ~debug_exprs:[]
    (sprintf "Int.equal (Obj.size (Obj.repr r)) %d" expected_size);
  print_newline ()

let test_record_access ty ~local =
  type_section ty;
  let stack_if_local = if local then "stack_ " else "" in
  line "let r = %s%s in" stack_if_local (Type.value_code ty 0);
  if not local then line "ref_to_force_heap_allocation := P r;";
  line "(* 1. Test field get *)";
  let fields =
    match ty with
    | Record { name = _; fields; boxing = Boxed } -> fields
    | _ -> invalid_arg "expected boxed record"
  in
  List.iter fields ~f:(fun (lbl, fld_t) ->
      List.iter (Type.unboxed_paths_by_depth fld_t)
        ~f:(fun (depth, unboxed_paths) ->
          line "(* Paths of depth %d *)" (depth + 1);
          List.iter unboxed_paths ~f:(fun unboxed_path ->
              line "(* .%s%s *)" lbl (Path.to_string unboxed_path);
              let full_path = Path.Field lbl :: unboxed_path in
              let sub_ty = Type.follow_path ty full_path in
              line "let actual = r%s in" (Path.to_string full_path);
              line "let expected = %s in"
                (Type.value_code sub_ty
                   (Type.num_subvals_left_of_path ty full_path)
                );
              seq_assert ~debug_exprs:[]
                (sprintf "%s actual expected" (Type.eq_code sub_ty))
          )
      )
  );
  line "Gc.compact ();";
  line "(* 2. Test field set *)";
  line "(* Change [r] to [next_r] one field at a time *)";
  line "let eq = %s in" (Type.eq_code ty);
  (* CR layouts v7.1: test "fine-grained" record sets once we support those *)
  line "let next_r = %s%s in" stack_if_local (Type.value_code ty 100);
  line "let r_expected = %s%s in" stack_if_local (Type.value_code ty 0);
  if not local then line "ref_to_force_heap_allocation := P next_r;";
  if not local then line "ref_to_force_heap_allocation := P r_expected;";
  List.iter fields ~f:(fun (lbl, fld_t) ->
      line "(* .%s *)" lbl;
      line "r.%s <- next_r.%s;" lbl lbl;
      line "let r_expected = { r_expected with %s = next_r.%s } in" lbl lbl;
      seq_assert ~debug_exprs:[] "eq r r_expected";
      line "Gc.compact ();"
  );
  line "(* 3. Test deep matching *)";
  let rec mk_deep_record_el_pat (ty : Type.t) : string option =
    match ty with
    | Record { fields; boxing = Unboxed } ->
      let field_pats =
        List.map fields ~f:(fun (s, fld_ty) ->
            match mk_deep_record_el_pat fld_ty with
            | Some pat -> sprintf "%s = %s" s pat
            | None -> s
        )
      in
      Some (sprintf "#{ %s }" (String.concat ~sep:"; " field_pats))
    | _ -> None
  in
  (* Make a pattern for a nested record that binds a variable for each field
     name (which are guaranteed to be distinct by [Metaprogramming.Type_naming].
     If deep, then also binds fields of contained unboxed records. *)
  let rec mk_record_pat (ty : Type.t) ~deep =
    match ty with
    | Record { fields; boxing = Boxed } ->
      let field_pats =
        List.map fields ~f:(fun (s, fld_ty) ->
            if deep
            then
              match mk_deep_record_el_pat fld_ty with
              | Some pat -> sprintf "%s = %s" s pat
              | None -> s
            else s
        )
      in
      sprintf "{ %s }" (String.concat ~sep:"; " field_pats)
    | _ -> invalid_arg "expected boxed record"
  in
  let pat = mk_record_pat ty ~deep:true in
  line "let %s = r in" pat;
  List.iter fields ~f:(fun (lbl, fld_ty) ->
      List.iter (Type.unboxed_paths_by_depth fld_ty)
        ~f:(fun (depth, unboxed_paths) ->
          List.iter unboxed_paths ~f:(fun unboxed_path ->
              let full_path = Path.Field lbl :: unboxed_path in
              let sub_ty = Type.follow_path ty full_path in
              (* [sub_ty] must be terminal for it to be bound*)
              if Option.is_none (mk_deep_record_el_pat sub_ty)
              then (
                let last_lbl =
                  match List.hd (List.rev full_path) with
                  | Path.Field lbl | Path.Unboxed_field lbl -> lbl
                in
                let sub_ty = Type.follow_path ty full_path in
                line "let expected_%s = %s in" last_lbl
                  (Type.value_code sub_ty
                     (Type.num_subvals_left_of_path ty full_path + 100)
                  );
                seq_assert ~debug_exprs:[]
                  (sprintf "%s expected_%s %s" (Type.eq_code sub_ty) last_lbl
                     last_lbl
                  )
              )
          )
      )
  );
  line "Gc.compact ();";
  line "(* 4. Test shallow matching *)";
  let pat = mk_record_pat ty ~deep:false in
  line "let %s = r in" pat;
  List.iter fields ~f:(fun (lbl, fld_ty) ->
      line "let expected_%s = %s in" lbl
        (Type.value_code fld_ty
           (Type.num_subvals_left_of_path ty [Path.Field lbl] + 100)
        );
      seq_assert ~debug_exprs:[]
        (sprintf "%s expected_%s %s" (Type.eq_code fld_ty) lbl lbl)
  );
  print_newline ()

let toplevel_unit_block f =
  assert (Int.equal !indent 0);
  line "let to_run () =";
  with_indent (fun () ->
      f ();
      line "()"
  );
  line ";;";
  line "let () = to_run ();;";
  line ""

type test =
  | Record_size
  | Record_access of
      { local : bool;
        with_void : bool
      }

let main test ~bytecode =
  let types =
    match test with
    | Record_size ->
      List.filter_map interesting_type_trees
        ~f:Type_structure.boxed_record_containing_unboxed_records
    | Record_access { with_void = false; _ } ->
      List.filter_map interesting_type_trees
        ~f:Type_structure.boxed_record_containing_unboxed_records
      |> List.filter ~f:(fun ty -> not (Type_structure.contains_unit_u ty))
    | Record_access { with_void = true; _ } ->
      List.filter_map interesting_type_trees
        ~f:Type_structure.boxed_record_containing_unboxed_records
      |> List.filter ~f:Type_structure.contains_unit_u
  in
  let types =
    if bytecode
    then List.filter types ~f:(fun ty -> not (Type_structure.contains_vec128 ty))
    else types
  in
  let naming = Type_naming.empty in
  let (naming : Type_naming.t), (types : Type.t list) =
    List.fold_left_map types ~init:naming ~f:(fun naming ty ->
        Type_naming.add_names naming ty
    )
  in
  line
    {|(* TEST
 include stdlib_stable;
 include stdlib_beta;
 include stdlib_upstream_compatible;|};
  if bytecode
  then (
    line {| flags = "-extension layouts_alpha";|};
    line {| bytecode;|}
  )
  else (
    line {| modules = "stubs.c";|};
    line {| flags = "-extension simd_beta -extension layouts_alpha";|};
    line {| flambda2;|};
    line {| stack-allocation;|};
    line {| native;|}
  );
  line {|*)|};
  line "(** This is code generated by [test_generation.ml]. *)";
  line "";
  line {|[@@@warning "-23"]|};
  line "%s" (Metaprogramming.preamble ~bytecode);
  line "%s" preamble;
  List.iter (Type_naming.decls_code naming) ~f:(fun s -> line "%s" s);
  line "";
  begin
    match test with
    | Record_size ->
      List.iter types ~f:(fun ty ->
          toplevel_unit_block (fun () -> test_record_size ~bytecode ty)
      )
    | Record_access { local } ->
      List.iter types ~f:(fun ty ->
          toplevel_unit_block (fun () -> test_record_access ~local ty)
      )
  end;
  line "for i = 1 to %d do" !test_id;
  with_indent (fun () ->
      line
        {|if not (Int_set.mem i !tests_run) then failwithf "test %%d not run" i|}
  );
  line "done;;";
  print_in_test "All tests passed."

let tests =
  [ "record_size", Record_size;
    "record_access", Record_access { local = false; with_void = false };
    "record_access_local", Record_access { local = true; with_void = false };
    "record_access_with_void", Record_access { local = false; with_void = true };
    ( "record_access_with_void_local",
      Record_access { local = true; with_void = true } )
  ]

let () =
  let fail () =
    failwith
      (sprintf "Usage %s <bytecode|native> <%s>" Sys.argv.(0)
         (String.concat ~sep:"|" (List.map ~f:fst tests))
      )
  in
  let bytecode, test_name =
    match Sys.argv with
    | [| _; "native"; test_name |] -> false, test_name
    | [| _; "bytecode"; test_name |] -> true, test_name
    | _ -> fail ()
  in
  let test =
    match List.assoc_opt test_name tests with
    | Some test -> test
    | None -> fail ()
  in
  main test ~bytecode

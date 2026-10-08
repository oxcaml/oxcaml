(*$ open Ppxlib_cinaps_helpers $*)
open! Import
open Common
open With_errors
module E = Extension
module EC = Extension.Context
module A = Attribute
module AC = Attribute.Context

module Rule = struct
  module Attr_replace = struct
    module Attribute_list = struct
      type ('a, _) t =
        | [] : ('a, unit) t
        | ( :: ) : ('a, 'b) Attribute.t * ('a, 'c) t -> ('a, 'b * 'c) t

      let rec to_packed_list : type a l. (a, l) t -> Attribute.packed list =
        function
        | [] -> []
        | x :: xs -> T x :: to_packed_list xs
    end

    module Parsed_payload_list = struct
      type _ t = [] : unit t | ( :: ) : 'a option * 'b t -> ('a * 'b) t
    end

    type ('a, 'list) unpacked = {
      name : string;
      attributes : ('a, 'list) Attribute_list.t;
      expand :
        ctxt:Expansion_context.Base.t -> 'a -> 'list Parsed_payload_list.t -> 'a;
    }

    type 'a t = T : ('a, _) unpacked -> 'a t

    let name (T t) = t.name
  end

  module Attr_group_inline = struct
    type ('a, 'b, 'c) unpacked = {
      attribute : ('b, 'c) Attribute.t;
      expect : bool;
      expand :
        ctxt:Expansion_context.Deriver.t ->
        Asttypes.rec_flag ->
        'b list ->
        'c option list ->
        'a list;
    }

    type ('a, 'b) t = T : ('a, 'b, _) unpacked -> ('a, 'b) t

    let attr_name (T t) = Attribute.name t.attribute

    let split_normal_and_expect l =
      List.partition l ~f:(fun (T t) -> not t.expect)
  end

  module Attr_inline = struct
    type ('a, 'b, 'c) unpacked = {
      attribute : ('b, 'c) Attribute.t;
      expect : bool;
      expand : ctxt:Expansion_context.Deriver.t -> 'b -> 'c -> 'a list;
    }

    type ('a, 'b) t = T : ('a, 'b, _) unpacked -> ('a, 'b) t

    let attr_name (T t) = Attribute.name t.attribute

    let split_normal_and_expect l =
      List.partition l ~f:(fun (T t) -> not t.expect)
  end

  module Attr_floating_inline = struct
    type ('a, 'b) unpacked = {
      attribute : ('a, 'b) Attribute.Floating.t;
      expand : ctxt:Expansion_context.Deriver.t -> 'b -> 'a list;
    }

    type 'a t = T : ('a, _) unpacked -> 'a t

    let attr_name (T t) = Attribute.Floating.name t.attribute
  end

  module Special_function = struct
    type t = {
      name : string;
      ident : Longident.t;
      expand : Parsetree.expression -> Parsetree.expression option;
    }
  end

  module Constant_kind = struct
    type t = Float | Integer
  end

  module Constant = struct
    type t = {
      suffix : char;
      kind : Constant_kind.t;
      expand : Location.t -> string -> Parsetree.expression;
    }
  end

  module Field = struct
    type 'a t =
      | Extension : Extension.t t
      | Special_function : Special_function.t t
      | Constant : Constant.t t
      | Attr_replace : 'a EC.t -> 'a Attr_replace.t t
      | Attr_str_type_decl :
          (structure_item, type_declaration) Attr_group_inline.t t
      | Attr_sig_type_decl :
          (signature_item, type_declaration) Attr_group_inline.t t
      | Attr_str_module_type_decl :
          (structure_item, module_type_declaration) Attr_inline.t t
      | Attr_sig_module_type_decl :
          (signature_item, module_type_declaration) Attr_inline.t t
      | Attr_str_module_binding :
          (structure_item, module_binding) Attr_inline.t t
      | Attr_sig_module_declaration :
          (signature_item, module_declaration) Attr_inline.t t
      | Attr_str_type_ext : (structure_item, type_extension) Attr_inline.t t
      | Attr_sig_type_ext : (signature_item, type_extension) Attr_inline.t t
      | Attr_str_exception : (structure_item, type_exception) Attr_inline.t t
      | Attr_sig_exception : (signature_item, type_exception) Attr_inline.t t
      | Attr_str_class_type_decl :
          (structure_item, class_type_declaration) Attr_group_inline.t t
      | Attr_sig_class_type_decl :
          (signature_item, class_type_declaration) Attr_group_inline.t t
      | Attr_str_floating : structure_item Attr_floating_inline.t t
      | Attr_sig_floating : signature_item Attr_floating_inline.t t

    type (_, _) equality = Eq : ('a, 'a) equality | Ne : (_, _) equality

    let eq : type a b. a t -> b t -> (a, b) equality =
     fun a b ->
      match (a, b) with
      | Extension, Extension -> Eq
      | Special_function, Special_function -> Eq
      | Constant, Constant -> Eq
      | Attr_replace a, Attr_replace b -> (
          match EC.eq a b with Eq -> Eq | Ne -> Ne)
      | Attr_str_type_decl, Attr_str_type_decl -> Eq
      | Attr_sig_type_decl, Attr_sig_type_decl -> Eq
      | Attr_str_type_ext, Attr_str_type_ext -> Eq
      | Attr_sig_type_ext, Attr_sig_type_ext -> Eq
      | Attr_str_exception, Attr_str_exception -> Eq
      | Attr_sig_exception, Attr_sig_exception -> Eq
      | Attr_str_module_type_decl, Attr_str_module_type_decl -> Eq
      | Attr_str_module_binding, Attr_str_module_binding -> Eq
      | Attr_sig_module_declaration, Attr_sig_module_declaration -> Eq
      | Attr_sig_module_type_decl, Attr_sig_module_type_decl -> Eq
      | Attr_str_class_type_decl, Attr_str_class_type_decl -> Eq
      | Attr_sig_class_type_decl, Attr_sig_class_type_decl -> Eq
      | Attr_str_floating, Attr_str_floating -> Eq
      | Attr_sig_floating, Attr_sig_floating -> Eq
      | _ -> Ne
  end

  type t = T : 'a Field.t * 'a -> t

  type ('a, 'b, 'c) attr_group_inline =
    ('b, 'c) Attribute.t ->
    (ctxt:Expansion_context.Deriver.t ->
    Asttypes.rec_flag ->
    'b list ->
    'c option list ->
    'a list) ->
    t

  type ('a, 'b, 'c) attr_inline =
    ('b, 'c) Attribute.t ->
    (ctxt:Expansion_context.Deriver.t -> 'b -> 'c -> 'a list) ->
    t

  type ('item, 'parsed_payload) attr_floating_inline =
    ('item, 'parsed_payload) Attribute.Floating.t ->
    (ctxt:Expansion_context.Deriver.t -> 'parsed_payload -> 'item list) ->
    t

  let rec filter : type a. a Field.t -> t list -> a list =
   fun field l ->
    match l with
    | [] -> []
    | T (field', x) :: l -> (
        match Field.eq field field' with
        | Field.Eq -> x :: filter field l
        | Field.Ne -> filter field l)

  let extension ext = T (Extension, ext)

  let special_function id f =
    T (Special_function, { name = id; ident = Longident.parse id; expand = f })

  let special_function' ident f =
    T (Special_function, { name = Longident.name ident; ident; expand = f })

  let constant kind suffix expand = T (Constant, { suffix; kind; expand })

  let attr_replace name kind attribute expand =
    T
      ( Attr_replace kind,
        T
          {
            name;
            attributes = [ attribute ];
            expand =
              (fun ~ctxt item payload ->
                match payload with
                | [ None ] -> assert false
                | [ Some payload ] -> expand ~ctxt item payload);
          } )

  module Attr_multiple_replace = struct
    module Attribute_list = Attr_replace.Attribute_list
    module Parsed_payload_list = Attr_replace.Parsed_payload_list

    let attr_multiple_replace name kind attributes expand =
      T (Attr_replace kind, T { name; attributes; expand })
  end

  let attr_str_type_decl attribute expand =
    T (Attr_str_type_decl, T { attribute; expand; expect = false })

  let attr_sig_type_decl attribute expand =
    T (Attr_sig_type_decl, T { attribute; expand; expect = false })

  let attr_str_module_type_decl attribute expand =
    T (Attr_str_module_type_decl, T { attribute; expand; expect = false })

  let attr_sig_module_type_decl attribute expand =
    T (Attr_sig_module_type_decl, T { attribute; expand; expect = false })

  let attr_str_module_binding attribute expand =
    T (Attr_str_module_binding, T { attribute; expand; expect = false })

  let attr_sig_module_declaration attribute expand =
    T (Attr_sig_module_declaration, T { attribute; expand; expect = false })

  let attr_str_type_ext attribute expand =
    T (Attr_str_type_ext, T { attribute; expand; expect = false })

  let attr_sig_type_ext attribute expand =
    T (Attr_sig_type_ext, T { attribute; expand; expect = false })

  let attr_str_exception attribute expand =
    T (Attr_str_exception, T { attribute; expand; expect = false })

  let attr_sig_exception attribute expand =
    T (Attr_sig_exception, T { attribute; expand; expect = false })

  let attr_str_class_type_decl attribute expand =
    T (Attr_str_class_type_decl, T { attribute; expand; expect = false })

  let attr_sig_class_type_decl attribute expand =
    T (Attr_sig_class_type_decl, T { attribute; expand; expect = false })

  let attr_str_type_decl_expect attribute expand =
    T (Attr_str_type_decl, T { attribute; expand; expect = true })

  let attr_sig_type_decl_expect attribute expand =
    T (Attr_sig_type_decl, T { attribute; expand; expect = true })

  let attr_str_module_type_decl_expect attribute expand =
    T (Attr_str_module_type_decl, T { attribute; expand; expect = true })

  let attr_str_module_binding_expect attribute expand =
    T (Attr_str_module_binding, T { attribute; expand; expect = true })

  let attr_sig_module_declaration_expect attribute expand =
    T (Attr_sig_module_declaration, T { attribute; expand; expect = true })

  let attr_sig_module_type_decl_expect attribute expand =
    T (Attr_sig_module_type_decl, T { attribute; expand; expect = true })

  let attr_str_type_ext_expect attribute expand =
    T (Attr_str_type_ext, T { attribute; expand; expect = true })

  let attr_sig_type_ext_expect attribute expand =
    T (Attr_sig_type_ext, T { attribute; expand; expect = true })

  let attr_str_exception_expect attribute expand =
    T (Attr_str_exception, T { attribute; expand; expect = true })

  let attr_sig_exception_expect attribute expand =
    T (Attr_sig_exception, T { attribute; expand; expect = true })

  let attr_str_class_type_decl_expect attribute expand =
    T (Attr_str_class_type_decl, T { attribute; expand; expect = true })

  let attr_sig_class_type_decl_expect attribute expand =
    T (Attr_sig_class_type_decl, T { attribute; expand; expect = true })

  let attr_str_floating_expect_and_expand attribute expand =
    T (Attr_str_floating, T { attribute; expand })

  let attr_sig_floating_expect_and_expand attribute expand =
    T (Attr_sig_floating, T { attribute; expand })
end

module Generated_code_hook = struct
  type 'a single_or_many = Single of 'a | Many of 'a list

  type t = {
    f : 'a. 'a Extension.Context.t -> Location.t -> 'a single_or_many -> unit;
  }

  let nop = { f = (fun _ _ _ -> ()) }
  let replace t context loc x = t.f context loc x

  let insert_after t context (loc : Location.t) x =
    match x with
    | Many [] -> ()
    | _ -> t.f context { loc with loc_start = loc.loc_end } x
end

(* Used to insert error extensions *)
let wrap_extension : type a. loc:Location.t -> a EC.t -> a -> extension -> a =
 fun ~loc t original_node extension ->
  (* Prefixing constructors with the module path is necessary for OCaml < 4.07,
     see https://github.com/ocaml/ocaml/issues/6852 *)
  match t with
  | EC.Class_expr -> Ast_builder.Default.pcl_extension ~loc extension
  | EC.Class_field -> Ast_builder.Default.pcf_extension ~loc extension
  | EC.Class_type -> Ast_builder.Default.pcty_extension ~loc extension
  | EC.Class_type_field -> Ast_builder.Default.pctf_extension ~loc extension
  | EC.Core_type -> Ast_builder.Default.ptyp_extension ~loc extension
  | EC.Expression -> Ast_builder.Default.pexp_extension ~loc extension
  | EC.Module_expr -> Ast_builder.Default.pmod_extension ~loc extension
  | EC.Module_type -> Ast_builder.Default.pmty_extension ~loc extension
  | EC.Pattern -> Ast_builder.Default.ppat_extension ~loc extension
  | EC.Signature_item -> Ast_builder.Default.psig_extension ~loc extension []
  | EC.Structure_item -> Ast_builder.Default.pstr_extension ~loc extension []
  | EC.Ppx_import ->
      (* Insert the error in the type decl manifest *)
      let ptype_manifest =
        Some (Ast_builder.Default.ptyp_extension ~loc extension)
      in
      { original_node with ptype_manifest }

let exn_to_extension exn =
  let error = exn_to_loc_error exn in
  let loc = Location.Error.get_location error in
  let extension = Location.Error.to_extension error in
  (extension, loc)

let exn_to_error_extension context original_node exn =
  let extension, loc = exn_to_extension exn in
  wrap_extension ~loc context original_node extension

let exn_to_stri exn =
  let extension, loc = exn_to_extension exn in
  Ast_builder.Default.pstr_extension ~loc extension []

let exn_to_sigi exn =
  let extension, loc = exn_to_extension exn in
  Ast_builder.Default.psig_extension ~loc extension []

let handle_attr_replace_once context attrs item base_ctxt : 'a option t =
  let result =
    List.find_map attrs ~f:(fun (Rule.Attr_replace.T a) ->
        let rec get_attr_payloads : type l.
            ('a, l) Rule.Attr_replace.Attribute_list.t ->
            (bool * l Rule.Attr_replace.Parsed_payload_list.t) t = function
          | [] -> return (false, Rule.Attr_replace.Parsed_payload_list.[])
          | x :: xs ->
              (if Attribute.Context.equal context (Attribute.context x) then
                 return @@ Attribute.get x item
               else return None)
              >>= fun p ->
              get_attr_payloads xs >>| fun (any_attrs, ps) ->
              ( Option.is_some p || any_attrs,
                Rule.Attr_replace.Parsed_payload_list.(p :: ps) )
        in
        let (any_attrs, payloads), errors = get_attr_payloads a.attributes in
        if any_attrs then
          Some
            ( (payloads, errors) >>= fun payloads ->
              return
              @@ Attribute.remove_seen context
                   (Rule.Attr_replace.Attribute_list.to_packed_list a.attributes)
                   item
              >>| fun item -> a.expand ~ctxt:base_ctxt item payloads )
        else None)
  in
  match result with
  | Some (item, errors) -> (Some item, errors)
  | None -> (None, [])

let rec handle_attr_replace_str attrs item base_ctxt =
  (match item.pstr_desc with
    | Pstr_extension _ ->
        handle_attr_replace_once AC.Pstr_extension attrs item base_ctxt
    | Pstr_eval _ -> handle_attr_replace_once AC.Pstr_eval attrs item base_ctxt
    | _ -> return None)
  >>= function
  | Some item -> handle_attr_replace_str attrs item base_ctxt
  | None -> return item

let rec handle_attr_replace_sig attrs item base_ctxt =
  (match item.psig_desc with
    | Psig_extension _ ->
        handle_attr_replace_once AC.Psig_extension attrs item base_ctxt
    | _ -> return None)
  >>= function
  | Some item -> handle_attr_replace_sig attrs item base_ctxt
  | None -> return item

let rec map_node_rec attr_context attr_rules ext_context ts super_call loc
    base_ctxt x ~embed_errors =
  let ctxt =
    Expansion_context.Extension.make ~extension_point_loc:loc ~base:base_ctxt ()
  in
  handle_attr_replace_once attr_context attr_rules x base_ctxt >>= function
  | Some x ->
      map_node_rec attr_context attr_rules ext_context ts super_call loc
        base_ctxt x ~embed_errors
  | None -> (
      match EC.get_extension ext_context x with
      | None -> super_call base_ctxt x
      | Some (ext, attrs) -> (
          (try
             E.For_context.convert_res ts ~ctxt ext
             |> With_errors.of_result ~default:None
           with exn when embed_errors ->
             With_errors.return
               (Some (exn_to_error_extension ext_context x exn)))
          >>= fun converted ->
          match converted with
          | None -> super_call base_ctxt x
          | Some x ->
              EC.merge_attributes_res ext_context x attrs
              |> With_errors.of_result ~default:x
              >>= fun x ->
              map_node_rec attr_context attr_rules ext_context ts super_call loc
                base_ctxt x ~embed_errors))

let map_context : type a. a EC.t -> a AC.t = function
  | EC.Class_expr -> AC.Class_expr
  | EC.Class_field -> AC.Class_field
  | EC.Class_type -> AC.Class_type
  | EC.Class_type_field -> AC.Class_type_field
  | EC.Core_type -> AC.Core_type
  | EC.Expression -> AC.Expression
  | EC.Module_expr -> AC.Module_expr
  | EC.Module_type -> AC.Module_type
  | EC.Pattern -> AC.Pattern
  | EC.Signature_item | EC.Structure_item ->
      assert false
      (* These can't happen because all the items get handled together in structure and
         signature processing *)
  | EC.Ppx_import -> AC.Type_declaration

let map_node attr_rules ext_context ts super_call loc base_ctxt x ~hook
    ~embed_errors =
  let attr_context = map_context ext_context in
  let ctxt =
    Expansion_context.Extension.make ~extension_point_loc:loc ~base:base_ctxt ()
  in
  handle_attr_replace_once attr_context attr_rules x base_ctxt >>= function
  | Some x ->
      map_node_rec attr_context attr_rules ext_context ts super_call loc
        base_ctxt x ~embed_errors
  | None -> (
      match EC.get_extension ext_context x with
      | None -> super_call base_ctxt x
      | Some (ext, attrs) -> (
          (try
             E.For_context.convert_res ts ~ctxt ext
             |> With_errors.of_result ~default:None
           with exn when embed_errors ->
             With_errors.return
               (Some (exn_to_error_extension ext_context x exn)))
          >>= fun converted ->
          match converted with
          | None -> super_call base_ctxt x
          | Some x ->
              map_node_rec attr_context attr_rules ext_context ts super_call loc
                base_ctxt
                (EC.merge_attributes ext_context x attrs)
                ~embed_errors
              >>| fun generated_code ->
              Generated_code_hook.replace hook ext_context loc
                (Single generated_code);
              generated_code))

let rec map_nodes attr_rules ext_context ts super_call get_loc base_ctxt l ~hook
    ~embed_errors ~in_generated_code =
  let attr_context = map_context ext_context in
  match l with
  | [] -> return []
  | x :: l -> (
      handle_attr_replace_once attr_context attr_rules x base_ctxt >>= function
      | Some x ->
          map_nodes attr_rules ext_context ts super_call get_loc base_ctxt
            (x :: l) ~hook ~embed_errors ~in_generated_code
      | None -> (
          match EC.get_extension ext_context x with
          | None ->
              (* These two lets force the evaluation order, so that errors are reported in
             the same order as they appear in the source file. *)
              super_call base_ctxt x >>= fun x ->
              map_nodes attr_rules ext_context ts super_call get_loc base_ctxt l
                ~hook ~embed_errors ~in_generated_code
              >>| fun l -> x :: l
          | Some (ext, attrs) -> (
              let extension_point_loc = get_loc x in
              let ctxt =
                Expansion_context.Extension.make ~extension_point_loc
                  ~base:base_ctxt ()
              in
              (try
                 E.For_context.convert_inline_res ts ~ctxt ext
                 |> With_errors.of_result ~default:None
               with exn when embed_errors ->
                 With_errors.return
                   (Some [ exn_to_error_extension ext_context x exn ]))
              >>= function
              | None ->
                  super_call base_ctxt x >>= fun x ->
                  map_nodes attr_rules ext_context ts super_call get_loc
                    base_ctxt l ~hook ~embed_errors ~in_generated_code
                  >>| fun l -> x :: l
              | Some converted ->
                  ((), attributes_errors attrs) >>= fun () ->
                  map_nodes attr_rules ext_context ts super_call get_loc
                    base_ctxt converted ~hook ~embed_errors
                    ~in_generated_code:true
                  >>= fun generated_code ->
                  if not in_generated_code then
                    Generated_code_hook.replace hook ext_context
                      extension_point_loc (Many generated_code);
                  map_nodes attr_rules ext_context ts super_call get_loc
                    base_ctxt l ~hook ~embed_errors ~in_generated_code
                  >>| fun code -> generated_code @ code)))

let map_nodes = map_nodes ~in_generated_code:false

let table_of_special_functions special_functions =
  match
    List.map special_functions
      ~f:(fun { Rule.Special_function.ident; expand; _ } -> (ident, expand))
    (* We expect the lookup to fail most of the time, by making the table big (and
       sparse), we make it more likely to fail quickly *)
    |> Hashtbl.of_alist ~size:(max 1024 (List.length special_functions * 2))
  with
  | Ok table -> table
  | Error ident ->
      Printf.ksprintf invalid_arg
        "Context_free.V1.map_top_down: %s present twice in list of special \
         functions"
        (List.find_map_exn special_functions ~f:(fun r ->
             if Poly.equal r.ident ident then Some r.name else None))

(* [get_group attr l] returns the list of the attributes for each
   node in [l].
   If [l] is empty or if none of the nodes in [l] have an attribute attached,
   [get_group] returns [None].
   If [l] is not empty and at least one of the nodes in [l] has an attribue
   attached, [get_group] returns the equivalent of
   [Some (List.map ~f:(Attribute.get attr) l)]. *)
let rec get_group attr l =
  match l with
  | [] -> return None
  | x :: l -> (
      get_group attr l >>= fun group ->
      Attribute.get_res attr x |> of_result ~default:None >>| fun attr2 ->
      match (attr2, group) with
      | None, None -> None
      | None, Some vals -> Some (None :: vals)
      | Some value, None -> Some (Some value :: List.map l ~f:(fun _ -> None))
      | Some value, Some vals -> Some (Some value :: vals))

(* Same as [List.rev] then [List.concat] but expecting the input to be of length <= 2 *)
let rev_concat = function
  | [] -> []
  | [ x ] -> x
  | [ x; y ] -> y @ x
  | l -> List.concat (List.rev l)

let sort_attr_replace l =
  List.sort l ~cmp:(fun a b ->
      String.compare (Rule.Attr_replace.name a) (Rule.Attr_replace.name b))

let sort_attr_group_inline l =
  List.sort l ~cmp:(fun a b ->
      String.compare
        (Rule.Attr_group_inline.attr_name a)
        (Rule.Attr_group_inline.attr_name b))

let sort_attr_inline l =
  List.sort l ~cmp:(fun a b ->
      String.compare
        (Rule.Attr_inline.attr_name a)
        (Rule.Attr_inline.attr_name b))

let sort_attr_floating_inline l =
  List.sort l ~cmp:(fun a b ->
      String.compare
        (Rule.Attr_floating_inline.attr_name a)
        (Rule.Attr_floating_inline.attr_name b))

let context_free_attribute_modification ~loc =
  Error
    ( Location.Error.createf ~loc
        "A context-free rule deleted or added attribues of a str/sig item",
      [] )

(* Returns the code generated by attribute handlers. We don't remove these attributes, as
   another pass might interpret them later. For instance both ppx_deriving and
   ppxlib_deriving interprets [@@deriving] attributes.

   This complexity is horrible, but in practice we don't care as [attrs] is always a list
   of one element; it only has [@@deriving].
*)
let handle_attr_group_inline attrs rf ~items ~expanded_items ~loc ~base_ctxt
    ~embed_errors ~convert_exn =
  List.fold_left attrs ~init:(return [])
    ~f:(fun acc (Rule.Attr_group_inline.T group) ->
      acc >>= fun acc ->
      get_group group.attribute items >>= fun g1 ->
      get_group group.attribute expanded_items >>= fun g2 ->
      match (g1, g2) with
      | None, None -> return acc
      | None, Some _ | Some _, None ->
          context_free_attribute_modification ~loc |> of_result ~default:acc
      | Some values, Some _ -> (
          let ctxt =
            Expansion_context.Deriver.make ~derived_item_loc:loc
              ~inline:group.expect ~base:base_ctxt ()
          in
          try
            let expect_items = group.expand ~ctxt rf expanded_items values in
            return (expect_items :: acc)
          with exn when embed_errors ->
            let error_item = [ convert_exn exn ] in
            return (error_item :: acc)))

let handle_attr_inline attrs ~convert_exn ~item ~expanded_item ~loc ~base_ctxt
    ~embed_errors =
  List.fold_left attrs ~init:(return []) ~f:(fun acc (Rule.Attr_inline.T a) ->
      acc >>= fun acc ->
      Attribute.get_res a.attribute item |> of_result ~default:None
      >>= fun g1 ->
      Attribute.get_res a.attribute expanded_item |> of_result ~default:None
      >>= fun g2 ->
      match (g1, g2) with
      | None, None -> return acc
      | None, Some _ | Some _, None ->
          context_free_attribute_modification ~loc |> of_result ~default:acc
      | Some value, Some _ -> (
          let ctxt =
            Expansion_context.Deriver.make ~derived_item_loc:loc
              ~inline:a.expect ~base:base_ctxt ()
          in
          try
            let expect_items = a.expand ~ctxt expanded_item value in
            return (expect_items :: acc)
          with exn when embed_errors ->
            let error_item = [ convert_exn exn ] in
            return (error_item :: acc)))

let handle_attr_floating_inline attrs ~item ~loc ~base_ctxt ~embed_errors
    ~convert_exn =
  List.fold_left attrs ~init:(return [])
    ~f:(fun acc (Rule.Attr_floating_inline.T a) ->
      acc >>= fun acc ->
      Attribute.Floating.convert_attr_res a.attribute item
      |> of_result ~default:None
      >>= function
      | None -> return acc
      | Some value -> (
          let ctxt =
            Expansion_context.Deriver.make ~derived_item_loc:loc ~inline:true
              ~base:base_ctxt ()
          in
          try
            let expect_items = a.expand ~ctxt value in
            return (expect_items :: acc)
          with exn when embed_errors ->
            let error_item = [ convert_exn exn ] in
            return (error_item :: acc)))

module Expect_mismatch_handler = struct
  type t = {
    f : 'a. 'a Attribute.Floating.Context.t -> Location.t -> 'a list -> unit;
  }

  let nop = { f = (fun _ _ _ -> ()) }
end

(** Apply any code-path attributes to the context. *)
let with_context base_ctxt e =
  Attribute.get_res Ast_traverse.enter_value e |> of_result ~default:None
  >>= fun option ->
  match option with
  | None -> return (base_ctxt, e)
  | Some { loc; txt } ->
      Attribute.remove_seen_res Expression [ T Ast_traverse.enter_value ] e
      |> of_result ~default:e
      >>| fun e -> (Expansion_context.Base.enter_value ~loc txt base_ctxt, e)

class map_top_down ?(expect_mismatch_handler = Expect_mismatch_handler.nop)
  ?(generated_code_hook = Generated_code_hook.nop) ?(embed_errors = false) rules
  =
  let hook = generated_code_hook in

  let special_functions =
    Rule.filter Special_function rules |> table_of_special_functions
  in
  let constants =
    Rule.filter Constant rules
    |> List.map ~f:(fun (c : Rule.Constant.t) -> ((c.suffix, c.kind), c.expand))
    |> Hashtbl.of_alist_exn
  in
  let extensions = Rule.filter Extension rules in
  let class_expr = E.filter_by_context EC.class_expr extensions
  and class_field = E.filter_by_context EC.class_field extensions
  and class_type = E.filter_by_context EC.class_type extensions
  and class_type_field = E.filter_by_context EC.class_type_field extensions
  and core_type = E.filter_by_context EC.core_type extensions
  and expression = E.filter_by_context EC.expression extensions
  and module_expr = E.filter_by_context EC.module_expr extensions
  and module_type = E.filter_by_context EC.module_type extensions
  and pattern = E.filter_by_context EC.pattern extensions
  and signature_item = E.filter_by_context EC.signature_item extensions
  and structure_item = E.filter_by_context EC.structure_item extensions
  and ppx_import = E.filter_by_context EC.Ppx_import extensions in

  let attr_replace_class_expr =
    Rule.filter (Attr_replace EC.class_expr) rules |> sort_attr_replace
  and attr_replace_class_field =
    Rule.filter (Attr_replace EC.class_field) rules |> sort_attr_replace
  and attr_replace_class_type =
    Rule.filter (Attr_replace EC.class_type) rules |> sort_attr_replace
  and attr_replace_class_type_field =
    Rule.filter (Attr_replace EC.class_type_field) rules |> sort_attr_replace
  and attr_replace_core_type =
    Rule.filter (Attr_replace EC.core_type) rules |> sort_attr_replace
  and attr_replace_expression =
    Rule.filter (Attr_replace EC.expression) rules |> sort_attr_replace
  and attr_replace_module_expr =
    Rule.filter (Attr_replace EC.module_expr) rules |> sort_attr_replace
  and attr_replace_module_type =
    Rule.filter (Attr_replace EC.module_type) rules |> sort_attr_replace
  and attr_replace_pattern =
    Rule.filter (Attr_replace EC.pattern) rules |> sort_attr_replace
  and attr_replace_signature_item =
    Rule.filter (Attr_replace EC.signature_item) rules |> sort_attr_replace
  and attr_replace_structure_item =
    Rule.filter (Attr_replace EC.structure_item) rules |> sort_attr_replace
    (* Intentionally ignoring [EC.Ppx_import] *)
  in

  let attr_str_type_decls, attr_str_type_decls_expect =
    Rule.filter Attr_str_type_decl rules
    |> sort_attr_group_inline |> Rule.Attr_group_inline.split_normal_and_expect
  in
  let attr_sig_type_decls, attr_sig_type_decls_expect =
    Rule.filter Attr_sig_type_decl rules
    |> sort_attr_group_inline |> Rule.Attr_group_inline.split_normal_and_expect
  in

  let attr_str_module_type_decls, attr_str_module_type_decls_expect =
    Rule.filter Attr_str_module_type_decl rules
    |> sort_attr_inline |> Rule.Attr_inline.split_normal_and_expect
  in
  let attr_sig_module_type_decls, attr_sig_module_type_decls_expect =
    Rule.filter Attr_sig_module_type_decl rules
    |> sort_attr_inline |> Rule.Attr_inline.split_normal_and_expect
  in

  let attr_str_module_binding, attr_str_module_binding_expect =
    Rule.filter Attr_str_module_binding rules
    |> sort_attr_inline |> Rule.Attr_inline.split_normal_and_expect
  in
  let attr_sig_module_declaration, attr_sig_module_declaration_expect =
    Rule.filter Attr_sig_module_declaration rules
    |> sort_attr_inline |> Rule.Attr_inline.split_normal_and_expect
  in

  let attr_str_type_exts, attr_str_type_exts_expect =
    Rule.filter Attr_str_type_ext rules
    |> sort_attr_inline |> Rule.Attr_inline.split_normal_and_expect
  in
  let attr_sig_type_exts, attr_sig_type_exts_expect =
    Rule.filter Attr_sig_type_ext rules
    |> sort_attr_inline |> Rule.Attr_inline.split_normal_and_expect
  in

  let attr_str_exceptions, attr_str_exceptions_expect =
    Rule.filter Attr_str_exception rules
    |> sort_attr_inline |> Rule.Attr_inline.split_normal_and_expect
  in
  let attr_sig_exceptions, attr_sig_exceptions_expect =
    Rule.filter Attr_sig_exception rules
    |> sort_attr_inline |> Rule.Attr_inline.split_normal_and_expect
  in

  let attr_str_class_decls, attr_str_class_decls_expect =
    Rule.filter Attr_str_class_type_decl rules
    |> sort_attr_group_inline |> Rule.Attr_group_inline.split_normal_and_expect
  in
  let attr_sig_class_decls, attr_sig_class_decls_expect =
    Rule.filter Attr_sig_class_type_decl rules
    |> sort_attr_group_inline |> Rule.Attr_group_inline.split_normal_and_expect
  in

  let attr_str_floating_expect_and_expand =
    Rule.filter Attr_str_floating rules |> sort_attr_floating_inline
  in
  let attr_sig_floating_expect_and_expand =
    Rule.filter Attr_sig_floating rules |> sort_attr_floating_inline
  in

  let map_node = map_node ~hook ~embed_errors in
  let map_nodes = map_nodes ~hook ~embed_errors in
  let handle_attr_group_inline = handle_attr_group_inline ~embed_errors in
  let handle_attr_inline = handle_attr_inline ~embed_errors in
  let handle_attr_floating_inline = handle_attr_floating_inline ~embed_errors in

  object (self)
    inherit Ast_traverse.map_with_expansion_context_and_errors as super

    (* No point recursing into every location *)
    method! location _ x = return x

    method! core_type base_ctxt x =
      map_node attr_replace_core_type EC.core_type core_type super#core_type
        x.ptyp_loc base_ctxt x

    method! pattern base_ctxt x =
      map_node attr_replace_pattern EC.pattern pattern super#pattern x.ppat_loc
        base_ctxt x

    method! expression base_ctxt e =
      with_context base_ctxt e >>= fun (base_ctxt, e) ->
      let expanded =
        map_node attr_replace_expression EC.expression expression
          (fun _ e -> return e)
          e.pexp_loc base_ctxt e
      in
      expanded >>= fun e ->
      let expand_constant kind char text =
        match Hashtbl.find_opt constants (char, kind) with
        | None -> super#expression base_ctxt e
        | Some expand -> self#expression base_ctxt (expand e.pexp_loc text)
      in
      match e.pexp_desc with
      | Pexp_apply (({ pexp_desc = Pexp_ident id; _ } as func), args) -> (
          match Hashtbl.find_opt special_functions id.txt with
          | None ->
              self#pexp_apply_without_traversing_function base_ctxt e func args
          | Some pattern -> (
              let generated_code =
                try return (pattern e)
                with exn when embed_errors ->
                  return (Some (exn_to_error_extension EC.expression e exn))
              in
              generated_code >>= fun expr ->
              match expr with
              | None ->
                  self#pexp_apply_without_traversing_function base_ctxt e func
                    args
              | Some e -> self#expression base_ctxt e))
      | Pexp_ident id -> (
          match Hashtbl.find_opt special_functions id.txt with
          | None -> super#expression base_ctxt e
          | Some pattern -> (
              let generated_code =
                try return (pattern e)
                with exn when embed_errors ->
                  return (Some (exn_to_error_extension EC.expression e exn))
              in
              generated_code >>= fun expr ->
              match expr with
              | None -> super#expression base_ctxt e
              | Some e -> self#expression base_ctxt e))
      | Pexp_constant (Pconst_integer (s, Some c)) -> (
          try expand_constant Integer c s
          with exn when embed_errors ->
            return (exn_to_error_extension EC.expression e exn))
      | Pexp_constant (Pconst_float (s, Some c)) -> (
          try expand_constant Float c s
          with exn when embed_errors ->
            return (exn_to_error_extension EC.expression e exn))
      | _ -> super#expression base_ctxt e

    (* Pre-conditions:
       - e.pexp_desc = Pexp_apply(func, args)
       - func.pexp_desc = Pexp_ident _
    *)
    method private pexp_apply_without_traversing_function base_ctxt e func args
        =
      let { pexp_desc = _; pexp_loc; pexp_attributes; pexp_loc_stack } = e in
      let func =
        with_context base_ctxt func >>= fun (base_ctxt, func) ->
        let rec handle_attr_replace_fix replaced item =
          handle_attr_replace_once AC.expression attr_replace_expression item
            base_ctxt
          >>= function
          | Some item -> handle_attr_replace_fix true item
          | None -> return (replaced, item)
        in
        handle_attr_replace_fix false func >>= fun (replaced, func) ->
        match replaced with
        (* If the attribute replacement changed the func then we should traverse it after
           all. This might cause some weirdness if the attribute replacement doesn't
           actually change the name of the function, because the special_function handling
           code will get called again on the ident, but that feels mostly unavoidable. *)
        | true -> super#expression base_ctxt func
        | false ->
            let { pexp_desc; pexp_loc; pexp_attributes; pexp_loc_stack } =
              func
            in
            self#attributes base_ctxt pexp_attributes >>| fun pexp_attributes ->
            {
              pexp_desc;
              pexp_loc (* location doesn't need to be traversed *);
              pexp_attributes;
              pexp_loc_stack;
            }
      in
      func >>= fun func ->
      let args =
        List.map args ~f:(fun (lab, exp) ->
            self#expression base_ctxt exp >>| fun exp -> (lab, exp))
        |> combine_errors
      in
      args >>= fun args ->
      self#attributes base_ctxt pexp_attributes >>| fun pexp_attributes ->
      {
        pexp_loc;
        pexp_attributes;
        pexp_desc = Pexp_apply (func, args);
        pexp_loc_stack;
      }

    method! class_type base_ctxt x =
      map_node attr_replace_class_type EC.class_type class_type super#class_type
        x.pcty_loc base_ctxt x

    method! class_type_field base_ctxt x =
      map_node attr_replace_class_type_field EC.class_type_field
        class_type_field super#class_type_field x.pctf_loc base_ctxt x

    method! class_expr base_ctxt x =
      map_node attr_replace_class_expr EC.class_expr class_expr super#class_expr
        x.pcl_loc base_ctxt x

    method! class_field base_ctxt x =
      map_node attr_replace_class_field EC.class_field class_field
        super#class_field x.pcf_loc base_ctxt x

    method! module_type base_ctxt x =
      map_node attr_replace_module_type EC.module_type module_type
        super#module_type x.pmty_loc base_ctxt x

    method! module_expr base_ctxt x =
      ( (* Make sure code-path attribute is applied before expanding. *)
        Attribute.get_res Ast_traverse.enter_module x |> of_result ~default:None
      >>= function
        | None -> return (base_ctxt, x)
        | Some { loc; txt } ->
            Attribute.remove_seen_res Module_expr
              [ T Ast_traverse.enter_module ]
              x
            |> of_result ~default:x
            >>| fun x ->
            (Expansion_context.Base.enter_module ~loc txt base_ctxt, x) )
      >>= fun (base_ctxt, x) ->
      map_node attr_replace_module_expr EC.module_expr module_expr
        super#module_expr x.pmod_loc base_ctxt x

    method! structure_item base_ctxt x =
      map_node attr_replace_structure_item EC.structure_item structure_item
        super#structure_item x.pstr_loc base_ctxt x

    method! signature_item base_ctxt x =
      map_node attr_replace_signature_item EC.signature_item signature_item
        super#signature_item x.psig_loc base_ctxt x

    method! class_structure base_ctxt { pcstr_self; pcstr_fields } =
      self#pattern base_ctxt pcstr_self >>= fun pcstr_self ->
      map_nodes attr_replace_class_field EC.class_field class_field
        super#class_field
        (fun x -> x.pcf_loc)
        base_ctxt pcstr_fields
      >>| fun pcstr_fields -> { pcstr_self; pcstr_fields }

    method! type_declaration base_ctxt x =
      map_node [] EC.Ppx_import ppx_import super#type_declaration x.ptype_loc
        base_ctxt x

    method! class_signature base_ctxt { pcsig_self; pcsig_fields } =
      self#core_type base_ctxt pcsig_self >>= fun pcsig_self ->
      map_nodes attr_replace_class_type_field EC.class_type_field
        class_type_field super#class_type_field
        (fun x -> x.pctf_loc)
        base_ctxt pcsig_fields
      >>| fun pcsig_fields -> { pcsig_self; pcsig_fields }

    (* TODO: try to factorize #structure and #signature without meta-programming *)
    (*$*)
    method! structure base_ctxt st =
      let convert_exn = exn_to_stri in
      let rec with_extra_items item ~extra_items ~expect_items ~rest
          ~in_generated_code =
        loop (rev_concat extra_items) ~in_generated_code:true
        >>= fun extra_items ->
        if not in_generated_code then
          Generated_code_hook.insert_after hook Structure_item item.pstr_loc
            (Many extra_items);
        let original_rest = rest in
        loop rest ~in_generated_code >>= fun rest ->
        (match expect_items with
          | [] -> return ()
          | _ ->
              let expected = rev_concat expect_items in
              let pos = item.pstr_loc.loc_end in
              Code_matcher.match_structure_res original_rest ~pos ~expected
                ~mismatch_handler:(fun loc repl ->
                  expect_mismatch_handler.f Structure_item loc repl)
              |> of_result ~default:())
        >>| fun () -> item :: (extra_items @ rest)
      and loop st ~in_generated_code =
        match st with
        | [] -> return []
        | item :: rest -> (
            handle_attr_replace_str attr_replace_structure_item item base_ctxt
            >>= fun item ->
            let loc = item.pstr_loc in
            match item.pstr_desc with
            | Pstr_extension (ext, attrs) -> (
                let extension_point_loc = item.pstr_loc in
                let ctxt =
                  Expansion_context.Extension.make ~extension_point_loc
                    ~base:base_ctxt ()
                in
                E.For_context.convert_inline_res structure_item ~ctxt ext
                |> of_result ~default:None
                >>= function
                | None ->
                    super#structure_item base_ctxt item >>= fun item ->
                    self#structure base_ctxt rest >>| fun rest -> item :: rest
                | Some items ->
                    ((), attributes_errors attrs) >>= fun () ->
                    (* assert_no_attributes attrs; *)
                    loop items ~in_generated_code:true >>= fun items ->
                    if not in_generated_code then
                      Generated_code_hook.replace hook Structure_item
                        item.pstr_loc (Many items);
                    loop rest ~in_generated_code >>| fun rest -> items @ rest)
            | Pstr_attribute at ->
                handle_attr_floating_inline attr_str_floating_expect_and_expand
                  ~item:at ~loc ~base_ctxt ~convert_exn
                >>= fun expect_items_unexpanded ->
                List.map expect_items_unexpanded ~f:(self#structure base_ctxt)
                |> combine_errors
                >>= fun expect_items_expanded ->
                (* Shouldn't matter if we use [rev_concat] or [List.concat] here, there
                   should be only one (outer) list among [expect_items_expanded] unless
                   a single floating attribute is somehow registered twice. *)
                (match rev_concat expect_items_expanded with
                  | [] -> return ()
                  | expected ->
                      Code_matcher.match_structure_res rest
                        ~pos:item.pstr_loc.loc_end ~expected
                        ~mismatch_handler:
                          (expect_mismatch_handler.f Structure_item)
                      |> of_result ~default:())
                >>= fun () ->
                super#structure_item base_ctxt item >>= fun expanded_item ->
                loop rest ~in_generated_code >>| fun expanded_rest ->
                expanded_item :: expanded_rest
            | _ -> (
                super#structure_item base_ctxt item >>= fun expanded_item ->
                match (item.pstr_desc, expanded_item.pstr_desc) with
                | Pstr_type (rf, tds), Pstr_type (exp_rf, exp_tds) ->
                    (* No context-free rule can rewrite rec flags atm, this
                       assert acts as a failsafe in case it ever changes *)
                    assert (Poly.(rf = exp_rf));
                    handle_attr_group_inline attr_str_type_decls rf ~items:tds
                      ~expanded_items:exp_tds ~loc ~base_ctxt ~convert_exn
                    >>= fun extra_items ->
                    handle_attr_group_inline attr_str_type_decls_expect rf
                      ~items:tds ~expanded_items:exp_tds ~loc ~base_ctxt
                      ~convert_exn
                    >>= fun expect_items ->
                    with_extra_items expanded_item ~extra_items ~expect_items
                      ~rest ~in_generated_code
                | Pstr_modtype mtd, Pstr_modtype exp_mtd ->
                    handle_attr_inline attr_str_module_type_decls ~item:mtd
                      ~expanded_item:exp_mtd ~loc ~base_ctxt ~convert_exn
                    >>= fun extra_items ->
                    handle_attr_inline attr_str_module_type_decls_expect
                      ~item:mtd ~expanded_item:exp_mtd ~loc ~base_ctxt
                      ~convert_exn
                    >>= fun expect_items ->
                    with_extra_items expanded_item ~extra_items ~expect_items
                      ~rest ~in_generated_code
                | Pstr_typext te, Pstr_typext exp_te ->
                    handle_attr_inline attr_str_type_exts ~item:te
                      ~expanded_item:exp_te ~loc ~base_ctxt ~convert_exn
                    >>= fun extra_items ->
                    handle_attr_inline attr_str_type_exts_expect ~item:te
                      ~expanded_item:exp_te ~loc ~base_ctxt ~convert_exn
                    >>= fun expect_items ->
                    with_extra_items expanded_item ~extra_items ~expect_items
                      ~rest ~in_generated_code
                | Pstr_exception ec, Pstr_exception exp_ec ->
                    handle_attr_inline attr_str_exceptions ~item:ec
                      ~expanded_item:exp_ec ~loc ~base_ctxt ~convert_exn
                    >>= fun extra_items ->
                    handle_attr_inline attr_str_exceptions_expect ~item:ec
                      ~expanded_item:exp_ec ~loc ~base_ctxt ~convert_exn
                    >>= fun expect_items ->
                    with_extra_items expanded_item ~extra_items ~expect_items
                      ~rest ~in_generated_code
                | Pstr_module mb, Pstr_module exp_mb ->
                    handle_attr_inline attr_str_module_binding ~item:mb
                      ~expanded_item:exp_mb ~loc ~base_ctxt ~convert_exn
                    >>= fun extra_items ->
                    handle_attr_inline attr_str_module_binding_expect ~item:mb
                      ~expanded_item:exp_mb ~loc ~base_ctxt ~convert_exn
                    >>= fun expect_items ->
                    with_extra_items expanded_item ~extra_items ~expect_items
                      ~rest ~in_generated_code
                | Pstr_class_type cds, Pstr_class_type exp_cds ->
                    handle_attr_group_inline attr_str_class_decls Nonrecursive
                      ~items:cds ~expanded_items:exp_cds ~loc ~base_ctxt
                      ~convert_exn
                    >>= fun extra_items ->
                    handle_attr_group_inline attr_str_class_decls_expect
                      Nonrecursive ~items:cds ~expanded_items:exp_cds ~loc
                      ~base_ctxt ~convert_exn
                    >>= fun expect_items ->
                    with_extra_items expanded_item ~extra_items ~expect_items
                      ~rest ~in_generated_code
                | _, _ ->
                    self#structure base_ctxt rest >>| fun rest ->
                    expanded_item :: rest))
      in
      loop st ~in_generated_code:false

    method! signature base_ctxt sg =
      let convert_exn = exn_to_sigi in
      let rec with_extra_items item ~extra_items ~expect_items ~rest
          ~in_generated_code =
        loop (rev_concat extra_items) ~in_generated_code:true
        >>= fun extra_items ->
        if not in_generated_code then
          Generated_code_hook.insert_after hook Signature_item item.psig_loc
            (Many extra_items);
        let original_rest = rest in
        loop rest ~in_generated_code >>= fun rest ->
        (match expect_items with
          | [] -> return ()
          | _ ->
              let expected = rev_concat expect_items in
              let pos = item.psig_loc.loc_end in
              Code_matcher.match_signature_res original_rest ~pos ~expected
                ~mismatch_handler:(fun loc repl ->
                  expect_mismatch_handler.f Signature_item loc repl)
              |> of_result ~default:())
        >>| fun () -> item :: (extra_items @ rest)
      and loop sg ~in_generated_code =
        match sg with
        | [] -> return []
        | item :: rest -> (
            handle_attr_replace_sig attr_replace_signature_item item base_ctxt
            >>= fun item ->
            let loc = item.psig_loc in
            match item.psig_desc with
            | Psig_extension (ext, attrs) -> (
                let extension_point_loc = item.psig_loc in
                let ctxt =
                  Expansion_context.Extension.make ~extension_point_loc
                    ~base:base_ctxt ()
                in
                E.For_context.convert_inline_res signature_item ~ctxt ext
                |> of_result ~default:None
                >>= function
                | None ->
                    super#signature_item base_ctxt item >>= fun item ->
                    self#signature base_ctxt rest >>| fun rest -> item :: rest
                | Some items ->
                    ((), attributes_errors attrs) >>= fun () ->
                    (* assert_no_attributes attrs; *)
                    loop items ~in_generated_code:true >>= fun items ->
                    if not in_generated_code then
                      Generated_code_hook.replace hook Signature_item
                        item.psig_loc (Many items);
                    loop rest ~in_generated_code >>| fun rest -> items @ rest)
            | Psig_attribute at ->
                handle_attr_floating_inline attr_sig_floating_expect_and_expand
                  ~item:at ~loc ~base_ctxt ~convert_exn
                >>= fun expect_items_unexpanded ->
                List.map expect_items_unexpanded ~f:(self#signature base_ctxt)
                |> combine_errors
                >>= fun expect_items_expanded ->
                (* Shouldn't matter if we use [rev_concat] or [List.concat] here, there
                   should be only one (outer) list among [expect_items_expanded] unless
                   a single floating attribute is somehow registered twice. *)
                (match rev_concat expect_items_expanded with
                  | [] -> return ()
                  | expected ->
                      Code_matcher.match_signature_res rest
                        ~pos:item.psig_loc.loc_end ~expected
                        ~mismatch_handler:
                          (expect_mismatch_handler.f Signature_item)
                      |> of_result ~default:())
                >>= fun () ->
                super#signature_item base_ctxt item >>= fun expanded_item ->
                loop rest ~in_generated_code >>| fun expanded_rest ->
                expanded_item :: expanded_rest
            | _ -> (
                super#signature_item base_ctxt item >>= fun expanded_item ->
                match (item.psig_desc, expanded_item.psig_desc) with
                | Psig_type (rf, tds), Psig_type (exp_rf, exp_tds) ->
                    (* No context-free rule can rewrite rec flags atm, this
                       assert acts as a failsafe in case it ever changes *)
                    assert (Poly.(rf = exp_rf));
                    handle_attr_group_inline attr_sig_type_decls rf ~items:tds
                      ~expanded_items:exp_tds ~loc ~base_ctxt ~convert_exn
                    >>= fun extra_items ->
                    handle_attr_group_inline attr_sig_type_decls_expect rf
                      ~items:tds ~expanded_items:exp_tds ~loc ~base_ctxt
                      ~convert_exn
                    >>= fun expect_items ->
                    with_extra_items expanded_item ~extra_items ~expect_items
                      ~rest ~in_generated_code
                | Psig_modtype mtd, Psig_modtype exp_mtd ->
                    handle_attr_inline attr_sig_module_type_decls ~item:mtd
                      ~expanded_item:exp_mtd ~loc ~base_ctxt ~convert_exn
                    >>= fun extra_items ->
                    handle_attr_inline attr_sig_module_type_decls_expect
                      ~item:mtd ~expanded_item:exp_mtd ~loc ~base_ctxt
                      ~convert_exn
                    >>= fun expect_items ->
                    with_extra_items expanded_item ~extra_items ~expect_items
                      ~rest ~in_generated_code
                | Psig_typext te, Psig_typext exp_te ->
                    handle_attr_inline attr_sig_type_exts ~item:te
                      ~expanded_item:exp_te ~loc ~base_ctxt ~convert_exn
                    >>= fun extra_items ->
                    handle_attr_inline attr_sig_type_exts_expect ~item:te
                      ~expanded_item:exp_te ~loc ~base_ctxt ~convert_exn
                    >>= fun expect_items ->
                    with_extra_items expanded_item ~extra_items ~expect_items
                      ~rest ~in_generated_code
                | Psig_exception ec, Psig_exception exp_ec ->
                    handle_attr_inline attr_sig_exceptions ~item:ec
                      ~expanded_item:exp_ec ~loc ~base_ctxt ~convert_exn
                    >>= fun extra_items ->
                    handle_attr_inline attr_sig_exceptions_expect ~item:ec
                      ~expanded_item:exp_ec ~loc ~base_ctxt ~convert_exn
                    >>= fun expect_items ->
                    with_extra_items expanded_item ~extra_items ~expect_items
                      ~rest ~in_generated_code
                | Psig_module md, Psig_module exp_md ->
                    handle_attr_inline attr_sig_module_declaration ~item:md
                      ~expanded_item:exp_md ~loc ~base_ctxt ~convert_exn
                    >>= fun extra_items ->
                    handle_attr_inline attr_sig_module_declaration_expect
                      ~item:md ~expanded_item:exp_md ~loc ~base_ctxt
                      ~convert_exn
                    >>= fun expect_items ->
                    with_extra_items expanded_item ~extra_items ~expect_items
                      ~rest ~in_generated_code
                | Psig_class_type cds, Psig_class_type exp_cds ->
                    handle_attr_group_inline attr_sig_class_decls Nonrecursive
                      ~items:cds ~expanded_items:exp_cds ~loc ~base_ctxt
                      ~convert_exn
                    >>= fun extra_items ->
                    handle_attr_group_inline attr_sig_class_decls_expect
                      Nonrecursive ~items:cds ~expanded_items:exp_cds ~loc
                      ~base_ctxt ~convert_exn
                    >>= fun expect_items ->
                    with_extra_items expanded_item ~extra_items ~expect_items
                      ~rest ~in_generated_code
                | _, _ ->
                    self#signature base_ctxt rest >>| fun rest ->
                    expanded_item :: rest))
      in
      loop sg ~in_generated_code:false
  end

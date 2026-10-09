open Import

type repr =
  | Unit
  | Int of int
  | String of string
  | Bool of bool
  | Char of char
  | Array of repr list
  | Float of float
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Record of (string * repr) list
  | Constr of string * repr list
  | Tuple of repr list
  | List of repr list
  | Special of string

let pp_collection ~pp_elm ~open_ ~close ~sep fmt l =
  match l with
  | [] -> Format.fprintf fmt "%s%s" open_ close
  | hd :: tl ->
      Format.fprintf fmt "@[<hv>%s %a@," open_ pp_elm hd;
      List.iter tl ~f:(fun sv -> Format.fprintf fmt "%s %a@," sep pp_elm sv);
      Format.fprintf fmt "%s@]" close

type 'a pp = Format.formatter -> 'a -> unit

let rec pp_repr : repr pp =
 fun fmt repr ->
  match repr with
  | Unit -> Format.fprintf fmt "()"
  | Int i -> Format.fprintf fmt "%i" i
  | String s -> Format.fprintf fmt "%S" s
  | Special s -> Format.fprintf fmt "%s" s
  | Bool b -> Format.fprintf fmt "%B" b
  | Char c -> Format.fprintf fmt "%c" c
  | Float f -> Format.fprintf fmt "%f" f
  | Int32 i32 -> Format.fprintf fmt "%li" i32
  | Int64 i64 -> Format.fprintf fmt "%Li" i64
  | Nativeint ni -> Format.fprintf fmt "%ni" ni
  | Array l ->
      pp_collection ~pp_elm:pp_repr ~open_:"[|" ~close:"|]" ~sep:";" fmt l
  | Tuple l ->
      pp_collection ~pp_elm:pp_repr ~open_:"(" ~close:")" ~sep:"," fmt l
  | List l -> pp_collection ~pp_elm:pp_repr ~open_:"[" ~close:"]" ~sep:";" fmt l
  | Record fields ->
      pp_collection ~pp_elm:pp_field ~open_:"{" ~close:"}" ~sep:";" fmt fields
  | Constr (cname, []) -> Format.fprintf fmt "%s" cname
  | Constr (cname, [ (Constr (_, _ :: _) as x) ]) ->
      Format.fprintf fmt "@[<hv 2>%s@ (%a)@]" cname pp_repr x
  | Constr (cname, [ x ]) ->
      Format.fprintf fmt "@[<hv 2>%s@ %a@]" cname pp_repr x
  | Constr (cname, l) ->
      Format.fprintf fmt "@[<hv 2>%s@ %a@]" cname pp_repr (Tuple l)

and pp_field fmt (fname, repr) =
  Format.fprintf fmt "@[<hv 2>%s =@ %a@]" fname pp_repr repr

(* TODO: split into Printer and Lifter config*)
module Config = struct
  type loc_mode = [ `Short | `Full ]

  type t = {
    show_attrs : bool;
    show_locs : bool;
    loc_mode : loc_mode;
    printer : repr pp;
  }

  module Default = struct
    let show_attrs = false
    let show_locs = false
    let loc_mode = `Short
    let printer = pp_repr
  end

  let default =
    let open Default in
    { show_attrs; show_locs; loc_mode; printer = pp_repr }

  let make ?(show_attrs = Default.show_attrs) ?(show_locs = Default.show_locs)
      ?(loc_mode = Default.loc_mode) ?(printer = Default.printer) () =
    { show_attrs; show_locs; loc_mode; printer }
end

let cnum (pos : Lexing.position) = pos.pos_cnum - pos.pos_bol

class lift_repr =
  object (self)
    inherit [repr] Ast_traverse.lift as super
    val mutable config = Config.default
    method set_config new_config = config <- new_config
    method get_config () = config
    method unit () = Unit
    method int i = Int i
    method string s = String s
    method bool b = Bool b
    method char c = Char c
    method float f = Float f
    method int32 i32 = Int32 i32
    method int64 i64 = Int64 i64
    method nativeint ni = Nativeint ni
    method! list lift_a list = List (List.map ~f:lift_a list)
    method tuple res_list = Tuple res_list
    method record fields = Record fields
    method constr ctr res_list = Constr (ctr, res_list)

    method array lift_a array =
      Array (Array.map ~f:lift_a array |> Array.to_list)

    method other _a = Special "__"
    method! location_stack _ls = Special "__lstack"

    method! position pos =
      match (config.Config.show_locs, config.Config.loc_mode) with
      | true, `Full -> super#position pos
      | _, _ -> Special "__pos"

    method! loc lift_a a_loc =
      match config.Config.show_locs with
      | true -> super#loc lift_a a_loc
      | false -> lift_a a_loc.txt

    method! location loc =
      match (config.Config.show_locs, config.Config.loc_mode) with
      | false, _ -> Special "__loc"
      | true, `Full -> super#location loc
      | true, `Short ->
          let begin_line = loc.loc_start.pos_lnum in
          let begin_char = cnum loc.loc_start in
          let end_line = loc.loc_end.pos_lnum in
          let end_char = cnum loc.loc_end in
          let repr =
            if Int.equal begin_line end_line then
              Format.sprintf "l%ic%i..%i" begin_line begin_char end_char
            else
              Format.sprintf "l%ic%i..l%ic%i" begin_line begin_char end_line
                end_char
          in
          let with_ghost = if loc.loc_ghost then repr ^ "(g)" else repr in
          Special with_ghost

    method! attributes attrs =
      match config.Config.show_attrs with
      | false -> Special "__attrs"
      | true -> super#attributes attrs

    method lift_record_with_desc :
        'record 'desc.
        lift_desc:('desc -> repr) ->
        lift_record:('record -> repr) ->
        desc:'desc ->
        attrs:attributes ->
        'record ->
        repr =
      fun ~lift_desc ~lift_record ~desc ~attrs x ->
        match (config.show_locs, config.show_attrs, attrs) with
        | false, false, _ | false, true, [] -> lift_desc desc
        | _, true, _ | true, _, _ -> lift_record x

    method! core_type ct =
      self#lift_record_with_desc ~lift_desc:self#core_type_desc
        ~lift_record:super#core_type ~desc:ct.ptyp_desc
        ~attrs:ct.ptyp_attributes ct

    method! row_field rf =
      self#lift_record_with_desc ~lift_desc:self#row_field_desc
        ~lift_record:super#row_field ~desc:rf.prf_desc ~attrs:rf.prf_attributes
        rf

    method! object_field obf =
      self#lift_record_with_desc ~lift_desc:self#object_field_desc
        ~lift_record:super#object_field ~desc:obf.pof_desc
        ~attrs:obf.pof_attributes obf

    method! pattern pat =
      self#lift_record_with_desc ~lift_desc:self#pattern_desc
        ~lift_record:super#pattern ~desc:pat.ppat_desc
        ~attrs:pat.ppat_attributes pat

    method! expression exp =
      self#lift_record_with_desc ~lift_desc:self#expression_desc
        ~lift_record:super#expression ~desc:exp.pexp_desc
        ~attrs:exp.pexp_attributes exp

    method! class_type cty =
      self#lift_record_with_desc ~lift_desc:self#class_type_desc
        ~lift_record:super#class_type ~desc:cty.pcty_desc
        ~attrs:cty.pcty_attributes cty

    method! class_type_field ctf =
      self#lift_record_with_desc ~lift_desc:self#class_type_field_desc
        ~lift_record:super#class_type_field ~desc:ctf.pctf_desc
        ~attrs:ctf.pctf_attributes ctf

    method! class_expr cl =
      self#lift_record_with_desc ~lift_desc:self#class_expr_desc
        ~lift_record:super#class_expr ~desc:cl.pcl_desc ~attrs:cl.pcl_attributes
        cl

    method! class_field cf =
      self#lift_record_with_desc ~lift_desc:self#class_field_desc
        ~lift_record:super#class_field ~desc:cf.pcf_desc
        ~attrs:cf.pcf_attributes cf

    method! module_type mty =
      self#lift_record_with_desc ~lift_desc:self#module_type_desc
        ~lift_record:super#module_type ~desc:mty.pmty_desc
        ~attrs:mty.pmty_attributes mty

    method! module_expr mod_ =
      self#lift_record_with_desc ~lift_desc:self#module_expr_desc
        ~lift_record:super#module_expr ~desc:mod_.pmod_desc
        ~attrs:mod_.pmod_attributes mod_

    method! structure_item stri = self#structure_item_desc stri.pstr_desc
    method! signature_item sigi = self#signature_item_desc sigi.psig_desc

    method! structure str =
      match config.show_attrs with
      | true -> super#structure str
      | false ->
          List.filter
            ~f:(function
              | { pstr_desc = Pstr_attribute _; _ } -> false | _ -> true)
            str
          |> super#structure

    method! signature sig_ =
      match config.show_attrs with
      | true -> super#signature sig_
      | false ->
          List.filter
            ~f:(function
              | { psig_desc = Psig_attribute _; _ } -> false | _ -> true)
            sig_
          |> super#signature

    method! class_structure cstr =
      match config.show_attrs with
      | true -> super#class_structure cstr
      | false ->
          let pcstr_fields =
            List.filter
              ~f:(function
                | { pcf_desc = Pcf_attribute _; _ } -> false | _ -> true)
              cstr.pcstr_fields
          in
          super#class_structure { cstr with pcstr_fields }

    method! class_signature csig =
      match config.show_attrs with
      | true -> super#class_signature csig
      | false ->
          let pcsig_fields =
            List.filter
              ~f:(function
                | { pctf_desc = Pctf_attribute _; _ } -> false | _ -> true)
              csig.pcsig_fields
          in
          super#class_signature { csig with pcsig_fields }

    method! directive_argument dira =
      self#directive_argument_desc dira.pdira_desc

    method! rec_flag rec_flag =
      match rec_flag with
      | Nonrecursive -> Constr ("Nonrecursive", [])
      | Recursive -> Constr ("Recursive", [])

    method! direction_flag direction_flag =
      match direction_flag with
      | Upto -> Constr ("Upto", [])
      | Downto -> Constr ("Downto", [])

    method! private_flag private_flag =
      match private_flag with
      | Private -> Constr ("Private", [])
      | Public -> Constr ("Public", [])

    method! mutable_flag mutable_flag =
      match mutable_flag with
      | Mutable -> Constr ("Mutable", [])
      | Immutable -> Constr ("Immutable", [])

    method! virtual_flag virtual_flag =
      match virtual_flag with
      | Virtual -> Constr ("Virtual", [])
      | Concrete -> Constr ("Concrete", [])

    method! override_flag override_flag =
      match override_flag with
      | Override -> Constr ("Override", [])
      | Fresh -> Constr ("Fresh", [])

    method! closed_flag closed_flag =
      match closed_flag with
      | Closed -> Constr ("Closed", [])
      | Open -> Constr ("Open", [])

    method! variance variance =
      match variance with
      | Covariant -> Constr ("Covariant", [])
      | Contravariant -> Constr ("Contravariant", [])
      | NoVariance -> Constr ("NoVariance", [])

    method! injectivity injectivity =
      match injectivity with
      | Injective -> Constr ("Injective", [])
      | NoInjectivity -> Constr ("NoInjectivity", [])
  end

type 'a configurable = ?config:Config.t -> 'a pp
type 'a configured = 'a pp

module type S = sig
  type 'a printer

  val structure : structure printer
  val structure_item : structure_item printer
  val signature : signature printer
  val signature_item : signature_item printer
  val expression : expression printer
  val pattern : pattern printer
  val core_type : core_type printer
end

module type Conf = sig
  val config : Config.t
end

module type Configured = S with type 'a printer = 'a configured
module type Configurable = S with type 'a printer = 'a configurable

module Make (Conf : Conf) : Configured = struct
  type 'a printer = 'a configured

  let lsv =
    let lift_repr = new lift_repr in
    lift_repr#set_config Conf.config;
    lift_repr

  let structure fmt str = pp_repr fmt (lsv#structure str)
  let structure_item fmt str = pp_repr fmt (lsv#structure_item str)
  let signature fmt str = pp_repr fmt (lsv#signature str)
  let signature_item fmt str = pp_repr fmt (lsv#signature_item str)
  let expression fmt str = pp_repr fmt (lsv#expression str)
  let pattern fmt str = pp_repr fmt (lsv#pattern str)
  let core_type fmt str = pp_repr fmt (lsv#core_type str)
end

let make config =
  (module Make (struct
    let config = config
  end) : Configured)

module Default = Make (struct
  let config = Config.default
end)

type 'a printer = 'a configurable

let lift_repr = new lift_repr

let with_config ~config ~f =
  let old_config = lift_repr#get_config () in
  lift_repr#set_config config;
  let res = f () in
  lift_repr#set_config old_config;
  res

let pp_with_config (type a) (lifter : a -> repr) ?(config = Config.default) fmt
    (x : a) =
  with_config ~config ~f:(fun () -> config.printer fmt (lifter x))

let structure = pp_with_config lift_repr#structure
let structure_item = pp_with_config lift_repr#structure_item
let signature = pp_with_config lift_repr#signature
let signature_item = pp_with_config lift_repr#signature_item
let expression = pp_with_config lift_repr#expression
let pattern = pp_with_config lift_repr#pattern
let core_type = pp_with_config lift_repr#core_type

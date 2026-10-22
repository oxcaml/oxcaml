(* The meaning of keywords [strict] and [never_returns_normally] is defined in
   terms of abstract values as follows:

   relaxed (default): nor = Safe and exn = Top and div = Top strict: nor = Safe
   and exn = Safe and div = Safe never_returns_normally: nor = Bot and exn = Top
   and div = Top

   where [nor] means normal return of the call, [exn] means return via an
   exception, [div] means diverging (non-terminating) executions, and the
   meaning and order of elements is:

   Top may allocate Safe does not allocate on any execution paths Bot
   unreachable

   Using more than one keyword means intersection (i.e., meet of the elements,
   pointwise lifted to tuples), so we get the following:

   [@zero_alloc assume] nor = Safe and exn = Top and div = Top [@zero_alloc
   assume strict] nor = Safe and exn = Safe and div = Safe [@zero_alloc assume
   strict never_returns_normally] nor = Bot and exn = Safe and div = Safe
   [@zero_alloc assume never_returns_normally] nor = Bot and exn = Top and div =
   Top

   See [Value] and [Annotation] in [backend/checkmach.ml]. *)
(* CR gyorsh: should we move [Value] and [Annotation] here or maybe "utils" and
   use them directly, instead of the weird compare function that abstracts them?
   Perhaps we should translate "strict" and "never_returns_normally" directly
   into (nor,exn,div) *)

module Component = struct
  type 'top t =
    | Top of 'top
    | Safe
    | Bot

  let map ~f = function Top top -> Top (f top) | Safe -> Safe | Bot -> Bot

  let print ~print_top ppf = function
    | Bot -> Format.fprintf ppf "bot"
    | Safe -> Format.fprintf ppf "safe"
    | Top top -> print_top ppf top

  let compare ~compare_top c1 c2 =
    match c1, c2 with
    | Bot, Bot -> 0
    | Bot, _ -> -1
    | _, Bot -> 1
    | Safe, Safe -> 0
    | Safe, _ -> -1
    | _, Safe -> 1
    | Top t1, Top t2 -> compare_top t1 t2

  let join ~join_top c1 c2 =
    match c1, c2 with
    | Bot, Bot -> Bot
    | Safe, Safe -> Safe
    | Top t1, Top t2 -> Top (join_top t1 t2)
    | Safe, Bot | Bot, Safe -> Safe
    | Top _, Bot | Top _, Safe -> c1
    | Bot, Top _ | Safe, Top _ -> c2

  let meet ~meet_top c1 c2 =
    match c1, c2 with
    | Bot, Bot -> Bot
    | Safe, Safe -> Safe
    | Top t1, Top t2 -> Top (meet_top t1 t2)
    | Safe, Bot | Bot, Safe -> Bot
    | Top _, Bot | Bot, Top _ -> Bot
    | Top _, Safe | Safe, Top _ -> Safe

  let lessequal ~lessequal_top c1 c2 =
    match c1, c2 with
    | Bot, Bot -> true
    | Safe, Safe -> true
    | Top t1, Top t2 -> lessequal_top t1 t2
    | Bot, Safe -> true
    | Bot, Top _ -> true
    | Safe, Top _ -> true
    | Top _, (Bot | Safe) -> false
    | Safe, Bot -> false
end

module Tag = struct
  type t =
    | N
    | E
    | D

  let compare = Stdlib.compare

  let print = function N -> "nor" | E -> "exn" | D -> "div"
end

module Components = struct
  type 'component t =
    { nor : 'component;
      exn : 'component;
      div : 'component
    }

  let create f = { nor = f Tag.N; exn = f Tag.E; div = f Tag.D }

  let iter ~f { nor; exn; div } =
    f Tag.N nor;
    f Tag.E exn;
    f Tag.D div

  let map ~f { nor; exn; div } =
    { nor = f Tag.N nor; exn = f Tag.E exn; div = f Tag.D div }

  let map2 ~f lhs rhs =
    { nor = f Tag.N lhs.nor rhs.nor;
      exn = f Tag.E lhs.exn rhs.exn;
      div = f Tag.D lhs.div rhs.div
    }

  let lessequal f lhs rhs =
    f lhs.nor rhs.nor && f lhs.exn rhs.exn && f lhs.div rhs.div

  let compare f { nor = n1; exn = e1; div = d1 }
      { nor = n2; exn = e2; div = d2 } =
    let c = f n1 n2 in
    if c <> 0
    then c
    else
      let c = f e1 e2 in
      if c <> 0 then c else f d1 d2

  let one_hot = Fun.compose create ( = )

  let print print_component ppf { nor; exn; div } =
    Format.fprintf ppf "@[<hv 2>{ nor = %a;@ exn = %a;@ div = %a }@]"
      print_component nor print_component exn print_component div
end

module Relevant_components_by_argument_index = struct
  type t = bool Components.t Numbers.Int.Map.t

  let compare = Numbers.Int.Map.compare (Components.compare Bool.compare)

  let join =
    Numbers.Int.Map.union (fun _ c1 c2 ->
      Some (Components.map2 ~f:(fun _ r1 r2 -> r1 || r2) c1 c2))

  let meet t1 t2 =
    Numbers.Int.Map.merge
      (fun _argument_index c1 c2 ->
        match c1, c2 with
        | Some c1, Some c2 ->
          let c = Components.map2 ~f:(fun _tag r1 r2 -> r1 && r2) c1 c2 in
          if c.nor || c.exn || c.div then Some c else None
        | None, _ | _, None -> None)
      t1 t2

  let lessequal t1 t2 =
    Numbers.Int.Map.for_all
      (fun argument_index c1 ->
        match Numbers.Int.Map.find_opt argument_index t2 with
        | None -> false
        | Some c2 -> Components.lessequal (fun r1 r2 -> (not r1) || r2) c1 c2)
      t1

  let print_argument ppf (argument_index, (relevant : bool Components.t)) =
    let paths =
      List.filter_map
        (fun (is_relevant, path) -> if is_relevant then Some path else None)
        [ relevant.nor, "normal";
          relevant.exn, "exceptional";
          relevant.div, "diverging" ]
    in
    Format.fprintf ppf "argument #%d on %a paths" (argument_index + 1)
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "/")
         Format.pp_print_string)
      paths

  let print ppf t =
    Format.fprintf ppf "{@[<hov>%a@]}"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
         print_argument)
      (Numbers.Int.Map.bindings t)
end

module Allocation_sources = struct
  type 'args_only t =
    | Allocation_other_than_calling_arguments
    | No_allocation_other_than_calling_arguments of 'args_only

  let map ~f = function
    | Allocation_other_than_calling_arguments ->
      Allocation_other_than_calling_arguments
    | No_allocation_other_than_calling_arguments args_only ->
      No_allocation_other_than_calling_arguments (f args_only)

  let compare ~compare_args_only t1 t2 =
    match t1, t2 with
    | ( Allocation_other_than_calling_arguments,
        Allocation_other_than_calling_arguments ) -> 0
    | ( No_allocation_other_than_calling_arguments _,
        Allocation_other_than_calling_arguments ) -> -1
    | ( Allocation_other_than_calling_arguments,
        No_allocation_other_than_calling_arguments _ ) -> 1
    | ( No_allocation_other_than_calling_arguments a1,
        No_allocation_other_than_calling_arguments a2 ) ->
      compare_args_only a1 a2

  let lessequal ~lessequal_args_only t1 t2 =
    match t1, t2 with
    | _, Allocation_other_than_calling_arguments -> true
    | ( No_allocation_other_than_calling_arguments a1,
        No_allocation_other_than_calling_arguments a2 ) ->
      lessequal_args_only a1 a2
    | ( Allocation_other_than_calling_arguments,
        No_allocation_other_than_calling_arguments _ ) ->
      false

  let join ~join_args_only t1 t2 =
    match t1, t2 with
    | Allocation_other_than_calling_arguments, _
    | _, Allocation_other_than_calling_arguments ->
      Allocation_other_than_calling_arguments
    | ( No_allocation_other_than_calling_arguments a1,
        No_allocation_other_than_calling_arguments a2 ) ->
      No_allocation_other_than_calling_arguments (join_args_only a1 a2)

  let meet ~meet_args_only t1 t2 =
    match t1, t2 with
    | Allocation_other_than_calling_arguments, t
    | t, Allocation_other_than_calling_arguments ->
      t
    | ( No_allocation_other_than_calling_arguments a1,
        No_allocation_other_than_calling_arguments a2 ) ->
      No_allocation_other_than_calling_arguments (meet_args_only a1 a2)

  let other_than_calling_arguments = function
    | Allocation_other_than_calling_arguments -> true
    | No_allocation_other_than_calling_arguments _ -> false

  let top = Allocation_other_than_calling_arguments

  let print print_args_only ppf = function
    | Allocation_other_than_calling_arguments ->
      Format.fprintf ppf "Allocation_other_than_calling_arguments"
    | No_allocation_other_than_calling_arguments args_only ->
      Format.fprintf ppf
        "@[<hv>No_allocation_other_than_calling_arguments@ %a@]"
        print_args_only args_only
end

module type WS = sig
  type t

  val empty : t

  val join : t -> t -> t

  val meet : t -> t -> t

  val lessequal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

module type Allocation_sources = sig
  type t

  val top : t

  val argument : argument_index:int -> tag:Tag.t -> t
end

module type Component = sig
  type t

  type allocation_sources

  type witnesses

  val top :
    allocation_sources:allocation_sources ->
    witnesses:witnesses ->
    t

  val safe : t

  val bot : t

  val lessequal : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val compare : t -> t -> int

  val print : witnesses:bool -> Format.formatter -> t -> unit
end

module Make_component (Witnesses : WS) = struct
  (* keep in sync with "resolved" values in Checkmach. *)
  type t = Witnesses.t Component.t

  let bot = Component.Bot

  let top ~allocation_sources:() ~witnesses = Component.Top witnesses

  let safe = Component.Safe

  let join = Component.join ~join_top:Witnesses.join

  let meet = Component.meet ~meet_top:Witnesses.meet

  let lessequal = Component.lessequal ~lessequal_top:Witnesses.lessequal

  let compare = Component.compare ~compare_top:Witnesses.compare

  let print ~witnesses =
    let print_top ppf w =
      Format.fprintf ppf "top";
      if witnesses then Format.fprintf ppf " (%a)" Witnesses.print w
    in
    Component.print ~print_top
end

module Make_value
    (Allocation_sources : Allocation_sources)
    (Witnesses : WS)
    (V : Component with type allocation_sources := Allocation_sources.t and type witnesses := Witnesses.t) =
struct
  (** Lifts V to triples *)
  type t = V.t Components.t

  let bot = Components.create (fun _ignore_tag -> V.bot)

  let lessequal = Components.lessequal V.lessequal

  let join = Components.map2 ~f:(fun _ignore_tag -> V.join)

  let meet = Components.map2 ~f:(fun _ignore_tag -> V.meet)

  let normal_return = { bot with nor = V.safe }

  let exn_escape = { bot with exn = V.safe }

  let diverges = { bot with div = V.safe }

  let safe = Components.create (fun _ignore_tag -> V.safe)

  let top ~allocation_sources ~witnesses =
    Components.create (fun _ignore_tag -> V.top ~allocation_sources ~witnesses)

  let relaxed ~witnesses : t =
    { nor = V.safe;
      exn = V.top ~allocation_sources:Allocation_sources.top ~witnesses;
      div = V.top ~allocation_sources:Allocation_sources.top ~witnesses
    }

  let of_annotation ~strict ~never_returns_normally ~never_raises =
    let res = if strict then safe else relaxed ~witnesses:Witnesses.empty in
    let res = if never_raises then { res with exn = V.bot } else res in
    if never_returns_normally then { res with nor = V.bot } else res

  let print ~witnesses ppf ({ nor; exn; div } : t) =
    let pp = V.print ~witnesses in
    Format.fprintf ppf "{ nor=%a;@ exn=%a;@ div=%a }@," pp nor pp exn pp div

  let compare = Components.compare V.compare
end

module No_allocation_sources : (Allocation_sources with type t = unit) = struct
  type t = unit

  let top = ()

  let argument ~argument_index:_ ~tag:_ = ()
end

module Assume_info = struct
  module Witnesses = struct
    type t = unit

    let join _ _ = ()

    let lessequal _ _ = true

    let meet _ _ = ()

    let print _ _ = ()

    let empty = ()

    let compare _ _ = 0
  end

  module V = Make_component (Witnesses)
  module Value = Make_value (No_allocation_sources) (Witnesses) (V)

  type t =
    | No_assume
    | Assume of Value.t
    | Assume_inferred of Value.t
  (* CR ccasinghino: consider extending this time to also capture "check"
     attributes, and using it everywhere in typed tree instead of sometimes
     having a check_attribute and sometimes having this type. *)

  let compare t1 t2 =
    match t1, t2 with
    | No_assume, No_assume -> 0
    | Assume v1, Assume v2 -> Value.compare v1 v2
    | Assume_inferred v1, Assume_inferred v2 -> Value.compare v1 v2
    | No_assume, (Assume _ | Assume_inferred _) -> -1
    | Assume_inferred _, Assume _ -> -1
    | Assume _, Assume_inferred _ -> 1
    | (Assume _ | Assume_inferred _), No_assume -> 1

  let equal t1 t2 = compare t1 t2 = 0

  let print ppf = function
    | No_assume -> ()
    | Assume v -> Format.fprintf ppf "%a" (Value.print ~witnesses:false) v
    | Assume_inferred v ->
      Format.fprintf ppf "(inferred)%a" (Value.print ~witnesses:false) v

  let to_string v = Format.asprintf "%a" print v

  let join t1 t2 =
    match t1, t2 with
    | No_assume, No_assume -> No_assume
    | No_assume, (Assume _ | Assume_inferred _)
    | (Assume _ | Assume_inferred _), No_assume ->
      No_assume
    | Assume t1, Assume t2 -> Assume (Value.join t1 t2)
    | Assume_inferred t1, Assume_inferred t2 ->
      Assume_inferred (Value.join t1 t2)
    | Assume t1, Assume_inferred t2 | Assume_inferred t1, Assume t2 ->
      Assume_inferred (Value.join t1 t2)

  let meet t1 t2 =
    match t1, t2 with
    | No_assume, No_assume -> No_assume
    | No_assume, (Assume _ as t) | (Assume _ as t), No_assume -> t
    | No_assume, (Assume_inferred _ as t) | (Assume_inferred _ as t), No_assume
      ->
      t
    | Assume t1, Assume t2 -> Assume (Value.meet t1 t2)
    | Assume_inferred t1, Assume_inferred t2 ->
      Assume_inferred (Value.meet t1 t2)
    | Assume t1, Assume_inferred t2 -> Assume (Value.meet t1 t2)
    | Assume_inferred t1, Assume t2 -> Assume (Value.meet t1 t2)

  let none = No_assume

  let create ~strict ~never_returns_normally ~never_raises ~inferred =
    let v = Value.of_annotation ~strict ~never_returns_normally ~never_raises in
    match inferred with false -> Assume v | true -> Assume_inferred v

  let get_value t =
    match t with No_assume -> None | Assume v | Assume_inferred v -> Some v

  let is_none t =
    match t with No_assume -> true | Assume _ | Assume_inferred _ -> false

  let is_inferred t =
    match t with Assume_inferred _ -> true | Assume _ | No_assume -> false
end

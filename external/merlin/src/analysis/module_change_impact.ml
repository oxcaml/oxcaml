(* Change-impact engine: indexes one facts value, then answers exact and
   family queries over its dependency graph. *)

module Facts = Module_implementation_facts
module Context = Facts.Context
module Key = Facts.Key
module Uid = Shape.Uid
module Uid_map = Map.Make (Uid)
module Ctx_map = Map.Make (Context)
module Int_set = Set.Make (Int)

module Vec = struct
  type 'a t = { mutable data : 'a array; mutable len : int; dummy : 'a }

  let create dummy = { data = Array.make 16 dummy; len = 0; dummy }

  let push t x =
    if t.len = Array.length t.data then begin
      let data = Array.make (2 * t.len) t.dummy in
      Array.blit t.data 0 data 0 t.len;
      t.data <- data
    end;
    t.data.(t.len) <- x;
    t.len <- t.len + 1

  let get t i = t.data.(i)
  let set t i x = t.data.(i) <- x
  let length t = t.len
end

type desc = Atom of Context.t | App of int * int | Proj of int * Uid.t

module Sig_key = struct
  type t = App of int * int | Proj of int * Uid.t

  let compare a b =
    match (a, b) with
    | App (f1, a1), App (f2, a2) ->
      let c = Int.compare f1 f2 in
      if c <> 0 then c else Int.compare a1 a2
    | App _, Proj _ -> -1
    | Proj _, App _ -> 1
    | Proj (c1, u1), Proj (c2, u2) ->
      let c = Int.compare c1 c2 in
      if c <> 0 then c else Uid.compare u1 u2
end

module Sig_map = Map.Make (Sig_key)

module Key_repr = struct
  type t = Named of int * Uid.t | Anon of Uid.t

  let compare a b =
    match (a, b) with
    | Named (c1, u1), Named (c2, u2) ->
      let c = Int.compare c1 c2 in
      if c <> 0 then c else Uid.compare u1 u2
    | Named _, Anon _ -> -1
    | Anon _, Named _ -> 1
    | Anon u1, Anon u2 -> Uid.compare u1 u2
end

module Key_map = Map.Make (Key_repr)

type t =
  { descs : desc Vec.t;
    parent : int Vec.t;
    rank : int Vec.t;
    sig_class : int Vec.t;
    uses : int list Vec.t;
    use_size : int Vec.t;
    mutable atoms : int Ctx_map.t;
    mutable sigs : int Sig_map.t;
    mutable key_ids : int Key_map.t;
    key_witness : Key.t Vec.t;
    key_family : Uid.t option Vec.t;
    key_checks : Facts.Check.t list Vec.t;
    key_out : int list Vec.t;
    mutable families : Int_set.t Uid_map.t;
    mutable global_omissions : Facts.Omission.t list;
    mutable family_omissions : Facts.Omission.t list Uid_map.t;
    mutable comp_of : int array;
    mutable comp_keys : int list array;
    mutable comp_out : int list array;
    mutable comp_count : int;
    mutable condensation_edges : int;
    mutable edge_visits : int;
    mutable edges : int;
    mutable use_moves : int
  }

let find t i =
  let i = ref i in
  while Vec.get t.parent !i <> !i do
    let p = Vec.get t.parent !i in
    Vec.set t.parent !i (Vec.get t.parent p);
    i := p
  done;
  !i

let new_node t desc =
  let id = Vec.length t.descs in
  Vec.push t.descs desc;
  Vec.push t.parent id;
  Vec.push t.rank 0;
  Vec.push t.sig_class id;
  Vec.push t.uses [];
  Vec.push t.use_size 0;
  id

let add_use t root parent_node =
  Vec.set t.uses root (parent_node :: Vec.get t.uses root);
  Vec.set t.use_size root (Vec.get t.use_size root + 1)

let sig_id t i = Vec.get t.sig_class (find t i)

let signature_of t i =
  match Vec.get t.descs i with
  | Atom _ -> None
  | App (f, a) -> Some (Sig_key.App (sig_id t f, sig_id t a))
  | Proj (c, u) -> Some (Sig_key.Proj (sig_id t c, u))

let merge t a b =
  let pending = Queue.create () in
  Queue.add (a, b) pending;
  while not (Queue.is_empty pending) do
    let a, b = Queue.take pending in
    let ra = find t a and rb = find t b in
    if ra <> rb then begin
      let rep, absorbed =
        if Vec.get t.rank ra >= Vec.get t.rank rb then (ra, rb) else (rb, ra)
      in
      if Vec.get t.rank rep = Vec.get t.rank absorbed then
        Vec.set t.rank rep (Vec.get t.rank rep + 1);
      let big, small =
        if Vec.get t.use_size ra >= Vec.get t.use_size rb then (ra, rb)
        else (rb, ra)
      in
      let surviving_sig = Vec.get t.sig_class big in
      let moved = Vec.get t.uses small in
      let combined = List.rev_append moved (Vec.get t.uses big) in
      let total = Vec.get t.use_size ra + Vec.get t.use_size rb in
      t.use_moves <- t.use_moves + Vec.get t.use_size small;
      Vec.set t.parent absorbed rep;
      Vec.set t.uses ra [];
      Vec.set t.uses rb [];
      Vec.set t.use_size ra 0;
      Vec.set t.use_size rb 0;
      Vec.set t.uses rep combined;
      Vec.set t.use_size rep total;
      Vec.set t.sig_class rep surviving_sig;
      List.iter
        (fun p ->
          match signature_of t p with
          | None -> ()
          | Some s -> (
            match Sig_map.find_opt s t.sigs with
            | Some q -> if find t q <> find t p then Queue.add (p, q) pending
            | None -> t.sigs <- Sig_map.add s p t.sigs))
        moved
    end
  done

let rec intern t (context : Context.t) =
  match context with
  | Def _ | Body _ | Site _ -> (
    match Ctx_map.find_opt context t.atoms with
    | Some id -> id
    | None ->
      let id = new_node t (Atom context) in
      t.atoms <- Ctx_map.add context id t.atoms;
      id)
  | App (functor_, argument) -> (
    let f = intern t functor_ in
    let a = intern t argument in
    let s = Sig_key.App (sig_id t f, sig_id t a) in
    match Sig_map.find_opt s t.sigs with
    | Some id -> id
    | None ->
      let id = new_node t (App (f, a)) in
      add_use t (find t f) id;
      add_use t (find t a) id;
      t.sigs <- Sig_map.add s id t.sigs;
      id)
  | Proj (inner, uid) -> (
    let c = intern t inner in
    let s = Sig_key.Proj (sig_id t c, uid) in
    match Sig_map.find_opt s t.sigs with
    | Some id -> id
    | None ->
      let id = new_node t (Proj (c, uid)) in
      add_use t (find t c) id;
      t.sigs <- Sig_map.add s id t.sigs;
      id)

let rec find_context_opt t (context : Context.t) =
  match context with
  | Def _ | Body _ | Site _ ->
    Option.map (find t) (Ctx_map.find_opt context t.atoms)
  | App (functor_, argument) -> (
    match (find_context_opt t functor_, find_context_opt t argument) with
    | Some f, Some a ->
      Option.map (find t)
        (Sig_map.find_opt
           (Sig_key.App (Vec.get t.sig_class f, Vec.get t.sig_class a))
           t.sigs)
    | (Some _ | None), _ -> None)
  | Proj (inner, uid) -> (
    match find_context_opt t inner with
    | Some c ->
      Option.map (find t)
        (Sig_map.find_opt (Sig_key.Proj (Vec.get t.sig_class c, uid)) t.sigs)
    | None -> None)

let key_repr t (key : Key.t) : Key_repr.t =
  match key with
  | Named { context; family_uid } ->
    Named (find t (intern t context), family_uid)
  | Anon { key_uid } -> Anon key_uid

let find_key_opt t (key : Key.t) =
  match (key : Key.t) with
  | Named { context; family_uid } ->
    Option.bind (find_context_opt t context) (fun root ->
        Key_map.find_opt (Key_repr.Named (root, family_uid)) t.key_ids)
  | Anon { key_uid } -> Key_map.find_opt (Key_repr.Anon key_uid) t.key_ids

let key_id t (key : Key.t) =
  let repr = key_repr t key in
  match Key_map.find_opt repr t.key_ids with
  | Some id ->
    if Key.compare key (Vec.get t.key_witness id) < 0 then
      Vec.set t.key_witness id key;
    id
  | None ->
    let id = Vec.length t.key_witness in
    t.key_ids <- Key_map.add repr id t.key_ids;
    Vec.push t.key_witness key;
    Vec.push t.key_family (Key.family key);
    Vec.push t.key_checks [];
    Vec.push t.key_out [];
    id

let observe_family t id =
  match Vec.get t.key_family id with
  | None -> ()
  | Some family ->
    let ids =
      match Uid_map.find_opt family t.families with
      | None -> Int_set.empty
      | Some ids -> ids
    in
    t.families <- Uid_map.add family (Int_set.add id ids) t.families

let build_condensation t =
  let n = Vec.length t.key_out in
  let visit_index = Array.make (max n 1) (-1) in
  let lowlink = Array.make (max n 1) 0 in
  let on_stack = Array.make (max n 1) false in
  let comp_of = Array.make (max n 1) (-1) in
  let scc_stack = ref [] in
  let next_index = ref 0 in
  let comp_count = ref 0 in
  let start v =
    visit_index.(v) <- !next_index;
    lowlink.(v) <- !next_index;
    incr next_index;
    scc_stack := v :: !scc_stack;
    on_stack.(v) <- true
  in
  for root = 0 to n - 1 do
    if visit_index.(root) = -1 then begin
      start root;
      let frames = ref [ (root, Vec.get t.key_out root) ] in
      while !frames <> [] do
        match !frames with
        | [] -> ()
        | (v, edges) :: rest -> (
          match edges with
          | w :: edges ->
            frames := (v, edges) :: rest;
            if visit_index.(w) = -1 then begin
              start w;
              frames := (w, Vec.get t.key_out w) :: !frames
            end
            else if on_stack.(w) then
              lowlink.(v) <- min lowlink.(v) visit_index.(w)
          | [] ->
            frames := rest;
            (match rest with
            | (parent_v, _) :: _ ->
              lowlink.(parent_v) <- min lowlink.(parent_v) lowlink.(v)
            | [] -> ());
            if lowlink.(v) = visit_index.(v) then begin
              let comp = !comp_count in
              incr comp_count;
              let continue = ref true in
              while !continue do
                match !scc_stack with
                | [] -> continue := false
                | w :: remaining ->
                  scc_stack := remaining;
                  on_stack.(w) <- false;
                  comp_of.(w) <- comp;
                  if w = v then continue := false
              done
            end)
      done
    end
  done;
  let comp_keys = Array.make (max !comp_count 1) [] in
  for id = n - 1 downto 0 do
    let c = comp_of.(id) in
    comp_keys.(c) <- id :: comp_keys.(c)
  done;
  let comp_out = Array.make (max !comp_count 1) [] in
  let condensation_edges = ref 0 in
  for id = 0 to n - 1 do
    let c = comp_of.(id) in
    List.iter
      (fun derived ->
        let d = comp_of.(derived) in
        if c <> d then comp_out.(c) <- d :: comp_out.(c))
      (Vec.get t.key_out id)
  done;
  for c = 0 to !comp_count - 1 do
    let out = List.sort_uniq Int.compare comp_out.(c) in
    comp_out.(c) <- out;
    condensation_edges := !condensation_edges + List.length out
  done;
  t.comp_of <- comp_of;
  t.comp_keys <- comp_keys;
  t.comp_out <- comp_out;
  t.comp_count <- !comp_count;
  t.condensation_edges <- !condensation_edges

let create (facts : Facts.t) =
  let t =
    { descs = Vec.create (Atom (Def Uid.internal_not_actually_unique));
      parent = Vec.create 0;
      rank = Vec.create 0;
      sig_class = Vec.create 0;
      uses = Vec.create [];
      use_size = Vec.create 0;
      atoms = Ctx_map.empty;
      sigs = Sig_map.empty;
      key_ids = Key_map.empty;
      key_witness =
        Vec.create (Key.Anon { key_uid = Uid.internal_not_actually_unique });
      key_family = Vec.create None;
      key_checks = Vec.create [];
      key_out = Vec.create [];
      families = Uid_map.empty;
      global_omissions = [];
      family_omissions = Uid_map.empty;
      comp_of = [||];
      comp_keys = [||];
      comp_out = [||];
      comp_count = 0;
      condensation_edges = 0;
      edge_visits = 0;
      edges = 0;
      use_moves = 0
    }
  in
  Facts.Context_equality.Set.iter
    (fun ({ left; right } : Facts.Context_equality.t) ->
      merge t (intern t left) (intern t right))
    facts.equalities;
  Facts.Check.Set.iter
    (fun (check : Facts.Check.t) ->
      let id = key_id t check.expectation in
      Vec.set t.key_checks id (check :: Vec.get t.key_checks id);
      observe_family t id)
    facts.checks;
  Facts.Dependency.Set.iter
    (fun ({ derived; source; reason } : Facts.Dependency.t) ->
      let derived_id = key_id t derived in
      let source_id = key_id t source in
      Vec.set t.key_out source_id (derived_id :: Vec.get t.key_out source_id);
      t.edges <- t.edges + 1;
      observe_family t derived_id;
      (match reason with
      | Definition ->
        Vec.set t.key_out derived_id (source_id :: Vec.get t.key_out derived_id);
        t.edges <- t.edges + 1
      | Alias
      | Include
      | With_constraint
      | Destructive_substitution
      | Module_type_of
      | Strengthening
      | Functor_type
      | Instance
      | Argument_member
      | Interface -> ());
      match reason with
      | Instance -> ()
      | Definition
      | Alias
      | Include
      | With_constraint
      | Destructive_substitution
      | Module_type_of
      | Strengthening
      | Functor_type
      | Argument_member
      | Interface -> observe_family t source_id)
    facts.dependencies;
  Facts.Omission.Set.iter
    (fun (omission : Facts.Omission.t) ->
      (match omission.affected with
      | None -> ()
      | Some affected -> observe_family t (key_id t affected));
      match omission.source with
      | None -> t.global_omissions <- omission :: t.global_omissions
      | Some family ->
        let omissions =
          match Uid_map.find_opt family t.family_omissions with
          | None -> []
          | Some omissions -> omissions
        in
        t.family_omissions <-
          Uid_map.add family (omission :: omissions) t.family_omissions)
    facts.omissions;
  for id = 0 to Vec.length t.key_out - 1 do
    Vec.set t.key_out id (List.sort_uniq Int.compare (Vec.get t.key_out id))
  done;
  build_condensation t;
  t

type impact = { witness : Key.t; check : Facts.Check.t }

type result = { impacts : impact list; omissions : Facts.Omission.t list }

let scoped_omissions t families =
  let omissions =
    Uid.Set.fold
      (fun family omissions ->
        match Uid_map.find_opt family t.family_omissions with
        | None -> omissions
        | Some scoped -> List.rev_append scoped omissions)
      families t.global_omissions
  in
  List.sort_uniq Facts.Omission.compare omissions

let impact_compare a b =
  let c = Key.compare a.witness b.witness in
  if c <> 0 then c else Facts.Check.compare a.check b.check

let query_seeds t ~queried_families seeds =
  let sets = Array.make (max t.comp_count 1) Int_set.empty in
  let witnesses = Array.of_list (List.map fst seeds) in
  List.iteri
    (fun w (_, id) ->
      let c = t.comp_of.(id) in
      sets.(c) <- Int_set.add w sets.(c))
    seeds;
  for c = t.comp_count - 1 downto 0 do
    if not (Int_set.is_empty sets.(c)) then
      List.iter
        (fun d ->
          t.edge_visits <- t.edge_visits + 1;
          sets.(d) <- Int_set.union sets.(d) sets.(c))
        t.comp_out.(c)
  done;
  let impacts = ref [] in
  let families = ref queried_families in
  for c = 0 to t.comp_count - 1 do
    let reaching = sets.(c) in
    if not (Int_set.is_empty reaching) then
      List.iter
        (fun id ->
          (match Vec.get t.key_family id with
          | None -> ()
          | Some family -> families := Uid.Set.add family !families);
          List.iter
            (fun check ->
              Int_set.iter
                (fun w ->
                  impacts := { witness = witnesses.(w); check } :: !impacts)
                reaching)
            (Vec.get t.key_checks id))
        t.comp_keys.(c)
  done;
  { impacts = List.sort_uniq impact_compare !impacts;
    omissions = scoped_omissions t !families
  }

let query_exact t key =
  match find_key_opt t key with
  | None ->
    let families =
      match Key.family key with
      | None -> Uid.Set.empty
      | Some family -> Uid.Set.singleton family
    in
    { impacts = []; omissions = scoped_omissions t families }
  | Some id -> query_seeds t ~queried_families:Uid.Set.empty [ (key, id) ]

let query_family t family =
  let queried_families = Uid.Set.singleton family in
  match Uid_map.find_opt family t.families with
  | None -> { impacts = []; omissions = scoped_omissions t queried_families }
  | Some ids ->
    let seeds =
      List.map (fun id -> (Vec.get t.key_witness id, id)) (Int_set.elements ids)
    in
    query_seeds t ~queried_families seeds

let global_omissions t =
  List.sort_uniq Facts.Omission.compare t.global_omissions

module For_testing = struct
  type counts =
    { context_nodes : int;
      keys : int;
      edges : int;
      condensation_edges : int;
      edge_visits : int;
      use_moves : int;
      max_parent_depth : int
    }

  let max_parent_depth t =
    let deepest = ref 0 in
    for i = 0 to Vec.length t.parent - 1 do
      let depth = ref 0 in
      let j = ref i in
      while Vec.get t.parent !j <> !j do
        incr depth;
        j := Vec.get t.parent !j
      done;
      if !depth > !deepest then deepest := !depth
    done;
    !deepest

  let counts t =
    { context_nodes = Vec.length t.descs;
      keys = Vec.length t.key_witness;
      edges = t.edges;
      condensation_edges = t.condensation_edges;
      edge_visits = t.edge_visits;
      use_moves = t.use_moves;
      max_parent_depth = max_parent_depth t
    }

  let merge_contexts t left right = merge t (intern t left) (intern t right)
end

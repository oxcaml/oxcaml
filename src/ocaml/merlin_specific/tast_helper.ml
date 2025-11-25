open Typedtree

module Pat = struct
  let pat_extra = []
  let pat_attributes = []

  let constant ?(loc = Location.none) pat_env pat_type c =
    let pat_desc = Tpat_constant c in
    { pat_desc;
      pat_loc = loc;
      pat_extra;
      pat_attributes;
      pat_type;
      pat_env;
      pat_unique_barrier = Unique_barrier.not_computed ()
    }

  let var ?loc uid pat_env pat_type str =
    let pat_loc =
      match loc with
      | None -> str.Asttypes.loc
      | Some loc -> loc
    in
    (* The level we use here isn't important - the constructed type is just used for
       printing and is never unified. *)
    let sort = Jkind.Sort.new_var ~level:(Ctype.get_current_level ()) in
    let mode = Mode.Value.newvar () in
    let pat_desc =
      Tpat_var (Ident.create_local str.Asttypes.txt, str, uid, sort, mode)
    in
    { pat_desc;
      pat_loc;
      pat_extra;
      pat_attributes;
      pat_type;
      pat_env;
      pat_unique_barrier = Unique_barrier.not_computed ()
    }

  let record ?(loc = Location.none) pat_env pat_type lst closed_flag =
    let pat_desc = Tpat_record (lst, closed_flag) in
    { pat_desc;
      pat_loc = loc;
      pat_extra;
      pat_attributes;
      pat_type;
      pat_env;
      pat_unique_barrier = Unique_barrier.not_computed ()
    }

  let tuple ?(loc = Location.none) pat_env pat_type lst =
    let pat_desc = Tpat_tuple lst in
    { pat_desc;
      pat_loc = loc;
      pat_extra;
      pat_attributes;
      pat_type;
      pat_env;
      pat_unique_barrier = Unique_barrier.not_computed ()
    }

  let construct ?(loc = Location.none) pat_env pat_type lid cstr_desc args
      locs_coretype =
    let pat_desc = Tpat_construct (lid, cstr_desc, args, locs_coretype) in
    { pat_desc;
      pat_loc = loc;
      pat_extra;
      pat_attributes;
      pat_type;
      pat_env;
      pat_unique_barrier = Unique_barrier.not_computed ()
    }

  let pat_or ?(loc = Location.none) ?row_desc pat_env pat_type p1 p2 =
    let pat_desc = Tpat_or (p1, p2, row_desc) in
    { pat_desc;
      pat_loc = loc;
      pat_extra;
      pat_attributes;
      pat_type;
      pat_env;
      pat_unique_barrier = Unique_barrier.not_computed ()
    }

  let variant ?(loc = Location.none) pat_env pat_type lbl sub rd =
    let pat_desc = Tpat_variant (lbl, sub, rd) in
    { pat_desc;
      pat_loc = loc;
      pat_extra;
      pat_attributes;
      pat_type;
      pat_env;
      pat_unique_barrier = Unique_barrier.not_computed ()
    }
end

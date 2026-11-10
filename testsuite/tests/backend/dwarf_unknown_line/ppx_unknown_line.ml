(* Give every application of [f_unknown] the location a ppx gives code
   whose line it does not know: [pos_lnum = -1] and [pos_bol = 0], so only
   [pos_cnum], the character offset in the file, is meaningful (see the
   comment on [Location.t]). *)

let unknown_line (p : Lexing.position) = { p with pos_lnum = -1; pos_bol = 0 }

let mapper =
  let open Ast_mapper in
  { default_mapper with
    expr =
      (fun m e ->
        let e = default_mapper.expr m e in
        match e.pexp_desc with
        | Pexp_apply
            ({ pexp_desc = Pexp_ident { txt = Lident "f_unknown"; _ }; _ }, _)
          ->
          let loc = e.pexp_loc in
          { e with
            pexp_loc =
              { loc with
                loc_start = unknown_line loc.loc_start;
                loc_end = unknown_line loc.loc_end
              }
          }
        | _ -> e)
  }

let () = Ast_mapper.run_main (fun _ -> mapper)

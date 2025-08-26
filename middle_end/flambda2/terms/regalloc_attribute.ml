(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Default
  | Cfg
  | Irc
  | Ls
  | Gi

let print ppf t =
  match t with
  | Default -> Format.pp_print_string ppf "Default"
  | Cfg -> Format.pp_print_string ppf "Cfg"
  | Irc -> Format.pp_print_string ppf "Irc"
  | Ls -> Format.pp_print_string ppf "Ls"
  | Gi -> Format.pp_print_string ppf "Gi"

let equal t1 t2 =
  match t1, t2 with
  | Default, Default | Cfg, Cfg | Irc, Irc | Ls, Ls | Gi, Gi -> true
  | (Default | Cfg | Irc | Ls | Gi), _ -> false

let is_default t = match t with Default -> true | Cfg | Irc | Ls | Gi -> false

let from_lambda (attr : Lambda.regalloc_attribute) =
  match attr with
  | Default_regalloc -> Default
  | Cfg_regalloc -> Cfg
  | Irc_regalloc -> Irc
  | Ls_regalloc -> Ls
  | Gi_regalloc -> Gi

let to_lambda t : Lambda.regalloc_attribute =
  match t with
  | Default -> Default_regalloc
  | Cfg -> Cfg_regalloc
  | Irc -> Irc_regalloc
  | Ls -> Ls_regalloc
  | Gi -> Gi_regalloc

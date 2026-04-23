(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  { arguments : Inlining_arguments.t;
    depth : int;
    stub_depth : int
  }

let increment_depth t ~is_stub ~by =
  { t with
    depth = t.depth + by;
    stub_depth = (if is_stub then t.stub_depth + 1 else 0)
  }

let default ~round =
  { arguments = Inlining_arguments.create ~round; depth = 0; stub_depth = 0 }

let create ~arguments ~depth ~stub_depth =
  if depth < 0 then Misc.fatal_errorf "depth must be >= 0: %d" depth;
  if stub_depth < 0
  then Misc.fatal_errorf "stub_depth must be >= 0: %d" stub_depth;
  { arguments; depth; stub_depth }

let [@ocamlformat "disable"] print ppf t =
  Format.fprintf ppf "@[<hov 1>(depth@ %d, stub_depth@ %d, arguments@ %a)@]"
    t.depth
    t.stub_depth
    Inlining_arguments.print t.arguments

let depth t = t.depth

let stub_depth t = t.stub_depth

let is_depth_exceeded t =
  t.depth >= Inlining_arguments.max_inlining_depth t.arguments

let meet t1 t2 =
  (* XXX is this depth calculation wrong? doesn't seem like a meet Or maybe this
     should be renamed to "combine" or something *)
  { depth = t1.depth + t2.depth;
    stub_depth = t1.stub_depth + t2.stub_depth;
    arguments = Inlining_arguments.meet t1.arguments t2.arguments
  }

let equal t1 t2 =
  t1.depth = t2.depth && Inlining_arguments.equal t1.arguments t2.arguments

let with_arguments arguments t = { t with arguments }

let arguments t = t.arguments

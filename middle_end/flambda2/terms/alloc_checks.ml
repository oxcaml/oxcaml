(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Nathanaëlle Courant, OCamlPro                       *)
(*                                                                        *)
(*   Copyright 2026 OCamlPro SAS                                          *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a per_exit =
  { normal : 'a;
    exn : 'a;
    notrace : 'a;
    div : 'a
  }

type check =
  | Forward
  | Close

type t = check per_exit

let map { normal; exn; notrace; div } ~f =
  { normal = f normal; exn = f exn; notrace = f notrace; div = f div }

let map2 { normal = normal1; exn = exn1; notrace = notrace1; div = div1 }
    { normal = normal2; exn = exn2; notrace = notrace2; div = div2 } ~f =
  { normal = f normal1 normal2;
    exn = f exn1 exn2;
    notrace = f notrace1 notrace2;
    div = f div1 div2
  }

let for_all { normal; exn; notrace; div } ~f =
  f normal && f exn && f notrace && f div

let compare_per_exit compare_elt
    { normal = normal1; exn = exn1; notrace = notrace1; div = div1 }
    { normal = normal2; exn = exn2; notrace = notrace2; div = div2 } =
  let c = compare_elt normal1 normal2 in
  if c <> 0
  then c
  else
    let c = compare_elt exn1 exn2 in
    if c <> 0
    then c
    else
      let c = compare_elt notrace1 notrace2 in
      if c <> 0 then c else compare_elt div1 div2

let meet_check check1 check2 =
  match check1, check2 with
  | Close, Close -> Close
  | (Forward | Close), (Forward | Close) -> Forward

let compare_check check1 check2 =
  match check1, check2 with
  | Forward, Forward | Close, Close -> 0
  | Forward, Close -> -1
  | Close, Forward -> 1

let meet t1 t2 = map2 t1 t2 ~f:meet_check

let print_check ppf = function
  | Forward -> Format.pp_print_string ppf "Forward"
  | Close -> Format.pp_print_string ppf "Close"

let [@ocamlformat "disable"] print_per_exit print_elt ppf
    { normal; exn; notrace; div } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(normal@ %a)@]@ \
      @[<hov 1>(exn@ %a)@]@ \
      @[<hov 1>(notrace@ %a)@]@ \
      @[<hov 1>(div@ %a)@]\
      )@]"
    print_elt normal
    print_elt exn
    print_elt notrace
    print_elt div

let print ppf t = print_per_exit print_check ppf t

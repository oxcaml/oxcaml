(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Not_yet_decided
  | Never_inline_attribute
  | Function_body_too_large of Code_size.t
  | Functor_body_too_large of Code_size.t
  | Stub
  | Attribute_inline
  | Small_function of
      { size : Code_size.t;
        small_function_size : Code_size.t
      }
  | Small_functor of
      { size : Code_size.t;
        small_functor_size : Code_size.t
      }
  | Speculatively_inlinable of
      { size : Code_size.t;
        small_function_size : Code_size.t;
        large_function_size : Code_size.t
      }
  | Speculatively_inlinable_functor of
      { size : Code_size.t;
        small_functor_size : Code_size.t;
        large_functor_size : Code_size.t
      }
  | Recursive
  | Jsir_inlining_disabled

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Not_yet_decided -> Format.fprintf ppf "Not_yet_decided"
  | Never_inline_attribute ->
    Format.fprintf ppf "Never_inline_attribute"
  | Function_body_too_large large_function_size ->
    Format.fprintf ppf
      "@[<hov 1>(Function_body_too_large@ %a)@]"
      Code_size.print large_function_size
  | Functor_body_too_large large_functor_size ->
    Format.fprintf ppf
      "@[<hov 1>(Functor_body_too_large@ %a)@]"
      Code_size.print large_functor_size
  | Stub ->
    Format.fprintf ppf "Stub"
  | Attribute_inline ->
    Format.fprintf ppf "Attribute_inline"
  | Small_function {size; small_function_size} ->
    Format.fprintf ppf
      "@[<hov 1>(Small_function@ \
        @[<hov 1>(size@ %a)@]@ \
        @[<hov 1>(small_function_size@ %a)@]\
        )@]"
      Code_size.print size
      Code_size.print small_function_size
  | Small_functor {size; small_functor_size} ->
    Format.fprintf ppf
      "@[<hov 1>(Small_functor@ \
        @[<hov 1>(size@ %a)@]@ \
        @[<hov 1>(small_functor_size@ %a)@]\
        )@]"
      Code_size.print size
      Code_size.print small_functor_size
  | Speculatively_inlinable {size;
                              small_function_size;
                              large_function_size} ->
    Format.fprintf ppf
      "@[<hov 1>(Speculatively_inlinable@ \
        @[<hov 1>(size@ %a)@]@ \
        @[<hov 1>(small_function_size@ %a)@]@ \
        @[<hov 1>(large_function_size@ %a)@]\
        )@]"
      Code_size.print size
      Code_size.print small_function_size
      Code_size.print large_function_size
  | Speculatively_inlinable_functor
      { size; small_functor_size; large_functor_size } ->
    Format.fprintf ppf
      "@[<hov 1>(Speculatively_inlinable_functor@ \
        @[<hov 1>(size@ %a)@]@ \
        @[<hov 1>(small_functor_size@ %a)@]@ \
        @[<hov 1>(large_functor_size@ %a)@]\
        )@]"
      Code_size.print size
      Code_size.print small_functor_size
      Code_size.print large_functor_size
  | Recursive ->
    Format.fprintf ppf "Recursive"
  | Jsir_inlining_disabled ->
    Format.fprintf ppf "Jsir_inlining_disabled"

let report_decision ppf t =
  match t with
  | Not_yet_decided -> Format.fprintf ppf "no decision has yet been made"
  | Never_inline_attribute ->
    Format.fprintf ppf "%a" Format.pp_print_text
      "the function has an attribute preventing its inlining"
  | Function_body_too_large large_function_size ->
    Format.fprintf ppf
      "the@ function's@ body@ is@ too@ large,@ more@ specifically,@ it@ is@ \
       larger@ than@ the@ large@ function@ size:@ %a"
      Code_size.print large_function_size
  | Functor_body_too_large large_functor_size ->
    Format.fprintf ppf
      "the@ functor's@ body@ is@ too@ large,@ more@ specifically,@ it@ is@ \
       larger@ than@ the@ large@ functor@ size:@ %a"
      Code_size.print large_functor_size
  | Stub -> Format.fprintf ppf "the@ function@ is@ a@ stub"
  | Attribute_inline ->
    Format.fprintf ppf
      "the@ function@ has@ an@ attribute@ forcing@ its@ inlining"
  | Small_function { size; small_function_size } ->
    Format.fprintf ppf
      "the@ function's@ body@ is@ smaller@ than@ the@ threshold@ size@ for@ \
       small@ functions: size=%a <= large@ function@ size=%a"
      Code_size.print size Code_size.print small_function_size
  | Small_functor { size; small_functor_size } ->
    Format.fprintf ppf
      "the@ functor's@ body@ is@ smaller@ than@ the@ threshold@ size@ for@ \
       small@ functors: size=%a <= small@ functor@ size=%a"
      Code_size.print size Code_size.print small_functor_size
  | Speculatively_inlinable { size; small_function_size; large_function_size }
    ->
    Format.fprintf ppf
      "the@ function's@ body@ is@ between@ the@ threshold@ size@ for@ small@ \
       functions and the@ threshold@ size@ for@ large@ functions: small@ \
       function@ size=%a < size=%a < large@ function@ size=%a"
      Code_size.print small_function_size Code_size.print size Code_size.print
      large_function_size
  | Speculatively_inlinable_functor
      { size; small_functor_size; large_functor_size } ->
    Format.fprintf ppf
      "the@ functor's@ body@ is@ between@ the@ threshold@ size@ for@ small@ \
       functors and the@ threshold@ size@ for@ large@ functors: small@ \
       functor@ size=%a < size=%a < large@ functor@ size=%a"
      Code_size.print small_functor_size Code_size.print size Code_size.print
      large_functor_size
  | Recursive -> Format.fprintf ppf "this@ function@ is@ recursive"
  | Jsir_inlining_disabled ->
    Format.fprintf ppf
      "function@ inlining@ is@ disabled@ for@ Js_of_ocaml@ translation"

type inlining_behaviour =
  | Cannot_be_inlined
  | Must_be_inlined
  | Could_possibly_be_inlined

let behaviour t =
  match t with
  | Not_yet_decided | Never_inline_attribute | Function_body_too_large _
  | Functor_body_too_large _ | Recursive | Jsir_inlining_disabled ->
    Cannot_be_inlined
  | Stub | Attribute_inline | Small_function _ | Small_functor _ ->
    Must_be_inlined
  | Speculatively_inlinable_functor _ | Speculatively_inlinable _ ->
    Could_possibly_be_inlined

let report fmt t =
  Format.fprintf fmt
    "@[<v>The function %s be inlined at its use-sites@ because @[<hov>%a@]@]"
    (match behaviour t with
    | Cannot_be_inlined -> "cannot"
    | Could_possibly_be_inlined -> "could"
    | Must_be_inlined -> "must")
    report_decision t

let must_be_inlined t =
  match behaviour t with
  | Must_be_inlined -> true
  | Cannot_be_inlined | Could_possibly_be_inlined -> false

let has_attribute_inline t =
  match t with
  | Attribute_inline -> true
  | Not_yet_decided | Never_inline_attribute | Function_body_too_large _
  | Functor_body_too_large _ | Stub | Small_function _ | Small_functor _
  | Speculatively_inlinable _ | Speculatively_inlinable_functor _ | Recursive
  | Jsir_inlining_disabled ->
    false

let cannot_be_inlined t =
  match behaviour t with
  | Cannot_be_inlined -> true
  | Must_be_inlined | Could_possibly_be_inlined -> false

let equal t1 t2 =
  match t1, t2 with
  | Not_yet_decided, Not_yet_decided
  | Never_inline_attribute, Never_inline_attribute
  | Stub, Stub
  | Attribute_inline, Attribute_inline ->
    true
  | Function_body_too_large size1, Function_body_too_large size2 ->
    Code_size.equal size1 size2
  | Functor_body_too_large size1, Functor_body_too_large size2 ->
    Code_size.equal size1 size2
  | ( Small_function { size = size1; small_function_size = small_function_size1 },
      Small_function
        { size = size2; small_function_size = small_function_size2 } ) ->
    Code_size.equal size1 size2
    && Code_size.equal small_function_size1 small_function_size2
  | ( Small_functor { size = size1; small_functor_size = small_functor_size1 },
      Small_functor { size = size2; small_functor_size = small_functor_size2 } )
    ->
    Code_size.equal size1 size2
    && Code_size.equal small_functor_size1 small_functor_size2
  | ( Speculatively_inlinable
        { size = size1;
          small_function_size = small_function_size1;
          large_function_size = large_function_size1
        },
      Speculatively_inlinable
        { size = size2;
          small_function_size = small_function_size2;
          large_function_size = large_function_size2
        } ) ->
    Code_size.equal size1 size2
    && Code_size.equal small_function_size1 small_function_size2
    && Code_size.equal large_function_size1 large_function_size2
  | ( Speculatively_inlinable_functor
        { size = size1;
          small_functor_size = small_functor_size1;
          large_functor_size = large_functor_size1
        },
      Speculatively_inlinable_functor
        { size = size2;
          small_functor_size = small_functor_size2;
          large_functor_size = large_functor_size2
        } ) ->
    Code_size.equal size1 size2
    && Code_size.equal small_functor_size1 small_functor_size2
    && Code_size.equal large_functor_size1 large_functor_size2
  | Recursive, Recursive -> true
  | Jsir_inlining_disabled, Jsir_inlining_disabled -> true
  | ( ( Not_yet_decided | Never_inline_attribute | Function_body_too_large _
      | Functor_body_too_large _ | Stub | Attribute_inline | Small_function _
      | Small_functor _ | Speculatively_inlinable _
      | Speculatively_inlinable_functor _ | Recursive | Jsir_inlining_disabled
        ),
      _ ) ->
    false

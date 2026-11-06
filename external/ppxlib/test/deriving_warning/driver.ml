open Ppxlib

let () =
  let unused_code_warnings = true in
  Deriving.add "zero_do_warn"
    ~str_type_decl:
      (Deriving.Generator.make_noarg ~unused_code_warnings
         (fun ~loc ~path:_ _ ->
           [%str
             module Zero = struct
               type t = T0
             end

             let zero = Zero.T0]))
    ~sig_type_decl:
      (Deriving.Generator.make_noarg ~unused_code_warnings
         (fun ~loc ~path:_ _ ->
           [%sig:
             module Zero : sig
               type t
             end

             val zero : Zero.t]))
  |> Deriving.ignore

let () =
  let unused_code_warnings = false in
  Deriving.add "one_no_warn"
    ~str_type_decl:
      (Deriving.Generator.make_noarg ~unused_code_warnings
         (fun ~loc ~path:_ _ ->
           [%str
             module One = struct
               type 'a t = T1 of 'a
             end

             let one = One.T1 zero]))
    ~sig_type_decl:
      (Deriving.Generator.make_noarg ~unused_code_warnings
         (fun ~loc ~path:_ _ ->
           [%sig:
             module One : sig
               type 'a t
             end

             val one : Zero.t One.t]))
  |> Deriving.ignore

let () =
  let unused_code_warnings = true in
  Deriving.add "two_do_warn"
    ~str_type_decl:
      (Deriving.Generator.make_noarg ~unused_code_warnings
         (fun ~loc ~path:_ _ ->
           [%str
             module Two = struct
               type ('a, 'b) t = T2 of 'a * 'b
             end

             let two = Two.T2 (zero, one)]))
    ~sig_type_decl:
      (Deriving.Generator.make_noarg ~unused_code_warnings
         (fun ~loc ~path:_ _ ->
           [%sig:
             module Two : sig
               type ('a, 'b) t
             end

             val two : (Zero.t, Zero.t One.t) Two.t]))
  |> Deriving.ignore

let () =
  let alias_do_warn =
    let unused_code_warnings = true in
    Deriving.add "alias_do_warn"
      ~str_type_decl:
        (Deriving.Generator.make_noarg ~unused_code_warnings
           (fun ~loc ~path:_ _ -> [%str let unit_one = ()]))
      ~sig_type_decl:
        (Deriving.Generator.make_noarg ~unused_code_warnings
           (fun ~loc ~path:_ _ -> [%sig: val unit_one : unit]))
  in
  let alias_no_warn =
    let unused_code_warnings = false in
    Deriving.add "alias_no_warn"
      ~str_type_decl:
        (Deriving.Generator.make_noarg ~unused_code_warnings
           (fun ~loc ~path:_ _ -> [%str let unit_two = unit_one]))
      ~sig_type_decl:
        (Deriving.Generator.make_noarg ~unused_code_warnings
           (fun ~loc ~path:_ _ -> [%sig: val unit_two : unit]))
  in
  (* The derivers are added from right to left *)
  Deriving.add_alias "alias_warn" [ alias_no_warn; alias_do_warn ]
  |> Deriving.ignore

let () = Driver.standalone ()

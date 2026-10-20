(* TEST
   flags += "-I ${ocamlsrcdir}/utils";
   only-default-codegen;
   expect.opt;
*)

let () =
  let print (f : Format.formatter) : Cmm.callee list option -> unit = function
    | None -> Format.fprintf f "- None@."
    | Some callees -> Format.fprintf f "- Some [%a]@." (Format.pp_print_list Printcmm.callee) callees
  in
  let rec visit (f : Format.formatter) (e : Cmm.expression) : unit =
    begin match e with
    | Cop (Capply { callees; _ }, _, _) -> print f callees
    | _ -> ()
    end;
    Cmm.iter_shallow (visit f) e
  in
  let hook : Cmm.phrase -> unit = function
    | Cdata _ -> ()
    | Cfunction { fun_name = { sym_name; _ }; fun_body; _ } ->
      if not (String.ends_with ~suffix:"__entry" sym_name) then
        Format.printf "%s:@.%a@." sym_name visit fun_body
  in
  Compiler_hooks.register Cmm
    (* eta-expanding because of an apparent toplevel bug: *)
    (fun phrase -> List.iter hook phrase)
;;

let apply f x = f x
[%%expect{|
camlTOP2__apply_6_11_code:
- Some [Argument { argument_index = 0 }]

val apply : ('a -> 'b) -> 'a -> 'b = <fun>
|}]

let alias f x = let g = f in g x
[%%expect{|
camlTOP3__alias_7_13_code:
- Some [Argument { argument_index = 0 }]

val alias : ('a -> 'b) -> 'a -> 'b = <fun>
|}]

let shadow f x = let f = ignore in f x
[%%expect{|
camlTOP4__shadow_8_16_code:

val shadow : 'a -> 'b -> unit = <fun>
|}]

(* "Tupled" functions (functions with a single tuple argument) are lowered to
   multi-argument functions: *)
let tupled (x, f) = f x
[%%expect{|
camlTOP5__tupled_10_18_code:
- Some [Argument { argument_index = 1 }]

val tupled : 'a * ('a -> 'b) -> 'b = <fun>
|}]

(* TEST
 flags+="-extension mode_alpha";
 expect;
*)

(* Allocation mode annotation on a module binding. *)
module M @ noalloc = struct
    let x = 42
end
[%%expect{|
|}]

(* A module at [noalloc_strict] should be usable where [noalloc] is expected
   (submoding within the Allocation axis). *)
let require_noalloc : (module sig end) @ noalloc -> unit = fun _ -> ()
[%%expect{|
|}]

(* Functor parameter annotated with an allocation mode. *)
module F (X : sig end @ noalloc) = struct end
[%%expect{|
|}]

(* Mismatched allocation mode should be rejected. *)
let _ =
    let module M @ alloc = struct end in
    let module _ = F(M) in
    ()
[%%expect{|
|}]

(* TEST
 flambda2;
 flags += "-O3 -flambda2-reaper";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-reaper;
 check-fexpr-dump;
*)

module type S = sig
  val x : int
end

let[@opaque] print_int x = ()

let () =
  let[@inline never] consume m =
    let module M = (val m : S) in
    print_int M.x
  in
  let module F = struct
    module Make (N : sig val n : int end) =
      (val
        let module M = struct
          let x = N.n
        end in
        consume (module M);
        (module M : S))
    [@@inline never]
  end in
  let module A = F.Make (struct let n = 0 end) in
  ()
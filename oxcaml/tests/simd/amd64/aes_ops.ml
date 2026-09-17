[@@@ocaml.warning "-unused-module"]

open Utils
open Builtins.AES

let () =
  Int64s.check_ints (fun lo hi ->
      let a = int64x2_of_int64s lo hi in
      let key = int64x2_of_int64s (Int64.lognot hi) lo in
      let check name ~result ~expect =
        (failmsg := fun () -> Printf.printf "%s %016Lx %016Lx\n%!" name lo hi);
        eq_int64x2 ~result ~expect
      in
      check "aesdec" ~result:(dec a key) ~expect:(dec_reference a key);
      check "aesdeclast" ~result:(declast a key)
        ~expect:(declast_reference a key);
      check "aesenc" ~result:(enc a key) ~expect:(enc_reference a key);
      check "aesenclast" ~result:(enclast a key)
        ~expect:(enclast_reference a key);
      check "aesimc" ~result:(imc a) ~expect:(imc_reference a);
      check "aeskeygenassist 0x00" ~result:(keygenassist 0x00 a)
        ~expect:(keygenassist_reference 0x00 a);
      check "aeskeygenassist 0x01" ~result:(keygenassist 0x01 a)
        ~expect:(keygenassist_reference 0x01 a);
      check "aeskeygenassist 0x1b" ~result:(keygenassist 0x1b a)
        ~expect:(keygenassist_reference 0x1b a);
      check "aeskeygenassist 0x36" ~result:(keygenassist 0x36 a)
        ~expect:(keygenassist_reference 0x36 a);
      check "aeskeygenassist 0x80" ~result:(keygenassist 0x80 a)
        ~expect:(keygenassist_reference 0x80 a);
      check "aeskeygenassist 0xff" ~result:(keygenassist 0xff a)
        ~expect:(keygenassist_reference 0xff a))

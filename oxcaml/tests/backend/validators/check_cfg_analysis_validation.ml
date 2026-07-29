open Cfg_intf.S
open Utils

let sequence = ref (InstructionId.make_sequence ())

let make_id () = InstructionId.get_and_incr !sequence

let terminator ?(arg = [||]) desc : Terminator.t =
  { id = make_id (); desc; arg; res = [||] }

let block ?(body = []) ?exn start terminator : Block.t =
  { start; body; terminator; exn }

let make_cfg blocks =
  sequence := InstructionId.make_sequence ();
  Cfg_desc.make_pre_regalloc
    { fun_args = [| int.(0); int.(1) |];
      blocks;
      fun_contains_calls = false;
      fun_ret_type = Cmm.typ_int
    }

let expect_fatal f =
  let fmt = Format.err_formatter in
  Format.pp_print_flush fmt ();
  let previous = Format.pp_get_formatter_out_functions fmt () in
  let sink = Format.formatter_of_buffer (Buffer.create 0) in
  Format.pp_set_formatter_out_functions fmt
    (Format.pp_get_formatter_out_functions sink ());
  let raised =
    Fun.protect
      ~finally:(fun () -> Format.pp_set_formatter_out_functions fmt previous)
      (fun () ->
        match f () with () -> false | exception Misc.Fatal_error -> true)
  in
  if not raised then failwith "expected a fatal error"

let test_unreachable () =
  let dead = new_label 13 in
  let cfg_with_infos =
    make_cfg
      [ block entry_label (terminator ~arg:[| int.(0) |] Return);
        block dead (terminator ~arg:[| int.(1) |] Return) ]
  in
  expect_fatal (fun () ->
      Cfg_reachability_validate.validate_reachability
        (Cfg_with_infos.cfg cfg_with_infos))

let () = test_unreachable ()

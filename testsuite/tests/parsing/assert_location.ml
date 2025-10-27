(* TEST_BELOW
(* Blank lines added here to preserve locations. *)
*)

let () = (
    assert false
  )

(* TEST
 exit_status = "2";
{native;}
{bytecode;}
{
  reference="${test_source_directory}/assert_location.js.reference";
  javascript;
}
*)

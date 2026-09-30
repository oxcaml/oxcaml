(* Build the test's position profile. Each non-comment line is: count
   unmangled_name discriminator body_hash ast_pos edge where edge is then, else,
   call, or an integer switch case; or count unmangled_name discriminator
   body_hash entry for the function's entry. The body hash is 8 hexadecimal
   digits, as -dfdo prints it. Every function named by a line is recorded as
   compiled with that body hash. *)
let () =
  match Sys.argv with
  | [| _; input; output |] ->
    let writer = Source_position_profile.Writer.create () in
    In_channel.with_open_text input (fun ic ->
        let rec loop () =
          match In_channel.input_line ic with
          | None -> ()
          | Some line ->
            let line = String.trim line in
            (if not (String.equal line "" || String.starts_with ~prefix:"#" line)
             then
               let record count unmangled_name discriminator body_hash
                   (position :
                     Fdo_counter.function_id ->
                     Fdo_counter.Function_body_hash.t ->
                     Fdo_counter.position) =
                 let function_id =
                   Fdo_counter.function_id ~unmangled_name
                     ~discriminator:(int_of_string discriminator)
                 in
                 let function_body_hash =
                   Fdo_counter.Function_body_hash.of_int32
                     (Int32.of_string ("0x" ^ body_hash))
                 in
                 Source_position_profile.Writer.add_body writer
                   ~hash:(Fdo_counter.hash_function_id function_id)
                   ~function_body_hash;
                 Source_position_profile.Writer.add_counter writer
                   ~counter:
                     { position = position function_id function_body_hash;
                       inlining_stack = []
                     }
                   ~count:(Int64.of_string count)
               in
               match String.split_on_char ' ' line with
               | [count; unmangled_name; discriminator; body_hash; "entry"] ->
                 record count unmangled_name discriminator body_hash
                   (fun function_id _ -> Fdo_counter.function_entry function_id)
               | [count; unmangled_name; discriminator; body_hash; ast_pos; edge]
                 ->
                 let edge : Fdo_counter.edge =
                   match edge with
                   | "then" -> Fdo_counter.Then
                   | "else" -> Else
                   | "call" -> Callsite
                   | n -> Switch_case (int_of_string n)
                 in
                 record count unmangled_name discriminator body_hash
                   (fun function_id function_body_hash ->
                     Fdo_counter.position ~function_id ~function_body_hash
                       ~ast_pos:(int_of_string ast_pos) ~edge)
               | _ -> failwith ("bad profile line: " ^ line));
            loop ()
        in
        loop ());
    Source_position_profile.Writer.write writer ~filename:output
  | _ ->
    prerr_endline "usage: gen_profile <input.txt> <output.fdo>";
    exit 2

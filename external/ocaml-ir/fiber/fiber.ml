type 'a t = 'a
let return t = t
let bind t ~f = f t
let map t ~f = f t
let join t = t
let all l = l
module Monad_infix =
  struct let (>>=) t f = bind t ~f
         let (>>|) t f = map t ~f end
include Monad_infix
module Let_syntax =
  struct
    let return = return
    include Monad_infix
    module Let_syntax =
      struct
        let return = return
        let bind = bind
        let map = map
        let both a b = (a, b)
        module Open_on_rhs = struct  end
      end
  end
let try_with f = try Ok (f ()) with | exn -> Error exn
let protect ~finally f =
  try let r = f () in (try finally () with | _ -> ()); r
  with | exn -> (finally (); raise exn)
let block_on f = f ()
let from_blocking f = return (f ())
module Sys =
  struct
    open Sys
    let file_exists x = if file_exists x then `Yes else `No
    external realpath : string -> string = "ocaml_ir_realpath"
    let get_cwd () = Unix.getcwd ()
  end
module Io =
  struct
    let read_file ~path =
      let fd = open_in path in
      let len = in_channel_length fd in
      let content = really_input_string fd len in close_in fd; content
    let create_file_write_content ~path ~content =
      let fd = open_out path in output_string fd content; close_out fd
  end
module Process = struct
  let run' ~prog ~args output =
    let inch =
      Unix.open_process_args_in prog (Array.of_list (prog :: args))
    in
    match
      protect
        (fun () -> output inch)
        ~finally:(fun () ->
          match Unix.close_process_in inch with
          | Unix.WEXITED 0 -> ()
          | Unix.WEXITED n -> raise (Sys_error ( Printf.sprintf "Process %s exited with code %d\n" prog n  ))
          | Unix.WSIGNALED signal ->
            raise (Sys_error (Printf.sprintf  "Process %s was killed by OCaml signal %d\n" prog signal ))
          | Unix.WSTOPPED _ -> assert false)
    with
    | x -> Ok x
    | exception exn -> Error exn
  ;;

  let fold_lines ~prog ~args ~init ~f () =
    let rec loop ~init inch =
      match In_channel.input_line inch with
      | None -> init
      | Some line -> loop ~init:(f init line) inch
    in
    run' ~prog ~args (loop ~init)
  ;;

  let run ~prog ~args () =
    run' ~prog ~args In_channel.input_all
  ;;
end
external mkdtemp : string -> string = "ocaml_ir_mkdtemp"

let with_temp_dir ?(template = "ocaml_ir") f =
  let dir = mkdtemp template |> Sys.realpath in
  protect
    (fun () ->
      try f dir with
      | exn -> raise exn)
    ~finally:(fun () -> Unix.system ("rm -rf " ^ Filename.quote dir) |> ignore)
;;

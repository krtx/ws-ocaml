open Unix

exception Error of string
let error err mes = raise (Error (err ^ ": " ^ mes))
let handle_error f x = try f x with Error err -> prerr_endline err; exit 2

let ws (client_sock, _) = assert false

let server () =
  let usage = Sys.argv.(0) ^ " <address> <port>" in
  if Array.length Sys.argv <= 2 then error "Usage: " usage;
  let spec = [] in
  let addr = ref "" in
  let port = ref "" in
  let anon_arg s = match !Arg.current with
    | 1 -> addr := s
    | 2 -> port := s
    | _ -> ()
  in
  Arg.parse spec anon_arg usage;
  let addr =
    try inet_addr_of_string !addr
    with Failure _ -> error !addr "Incorrect address"
  in
  let port =
    try int_of_string !port
    with Failure _ -> error !port "Incorrect port"
  in
  let allow_connection_errors f s =
    try f s with Exit | Unix_error (EPIPE, _, _) -> ()
  in
  let treat_connection _ =
    Misc.co_treatment (allow_connection_errors ws)
  in
  Misc.tcp_farm_server 10 treat_connection (ADDR_INET (addr, port))

let () = Misc.handle_unix_error (handle_error server) ()

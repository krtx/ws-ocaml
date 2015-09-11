open Unix
open Websocket

let ws (client_sock, _) =
  let cin  = in_channel_of_descr client_sock
  and cout = out_channel_of_descr client_sock in
  let ({ request_line; headers; _ } as req) = parse_request cin in
  begin
    match handshake_response req with
    | Ok    resp -> output_bytes cout resp
    | Error resp -> output_bytes cout resp
  end;
  close client_sock

let handle_error f x = try f x with Failure err -> prerr_endline err; exit 2

let server () =
  let usage = Sys.argv.(0) ^ " <address> <port>" in
  if Array.length Sys.argv <= 2 then failwith ("Usage: " ^ usage);
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
    with Failure _ -> failwith ("Incorrect address: " ^ !addr);
  in
  let port =
    try int_of_string !port
    with Failure _ -> failwith ("Incorrect port: " ^ !port)
  in
  let allow_connection_errors f s =
    try f s with Exit | Unix_error (EPIPE, _, _) -> ()
  in
  let treat_connection _ =
    Misc.co_treatment (allow_connection_errors ws)
  in
  Misc.tcp_farm_server 10 treat_connection (ADDR_INET (addr, port))

let () = handle_unix_error (handle_error server) ()

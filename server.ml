open Unix

let handshake handler (client_sock, _) =
  let cin = in_channel_of_descr client_sock
  and cout = out_channel_of_descr client_sock in
  begin
    try
      let response = Handshake.parse_request cin |> Handshake.make_response in
      output_bytes cout response;
      flush cout;
      handler client_sock
    with
      Handshake.Bad_request response ->
      output_bytes cout response;
      flush cout
  end;
  close client_sock

let run ?(max_connection=10) handler addr port =
  let addr =
    try inet_addr_of_string addr
    with Failure _ -> failwith ("Incorrect address: " ^ addr)
  and port =
    try int_of_string port
    with Failure _ -> failwith ("Incorrect port: " ^ port)
  in
  let allow_connection_errors f s =
    try f s with Exit | Unix_error (EPIPE, _, _) -> ()
  in
  let treat_connection _ =
    Misc.co_treatment (allow_connection_errors (handshake handler))
  in
  Misc.tcp_farm_server max_connection treat_connection (ADDR_INET (addr, port))

let handle_error f x = try f x with Failure err -> prerr_endline err; exit 2

let () =
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
  handle_unix_error (handle_error (run Echo.app !addr !port)) ()

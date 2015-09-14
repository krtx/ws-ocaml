open Unix
open Frame

type t = file_descr -> unit
type client = out_channel
type message = Text of bytes | Binary of bytes

let make ~on_message client_sock =
  let cin = in_channel_of_descr client_sock
  and cout = out_channel_of_descr client_sock in
  let buf = ref (Bytes.create 0) in
  try
    while true do
      let { fin; opcode; payload_data } = read_frame cin in
      if opcode = Continuation || opcode = Text || opcode = Binary
      then buf := Bytes.cat !buf payload_data;
      begin
        match opcode with
        | Continuation -> ()
        | Text -> if fin then on_message cout (Text !buf)
        | Binary -> if fin then on_message cout (Binary !buf)
        | Close ->
          output_bytes cout
            (to_bytes { fin = true
                      ; opcode = Close
                      ; payload_data = Bytes.sub payload_data 0 2 });
          flush cout;
          raise Exit
        | Ping ->
          output_bytes cout
            (to_bytes { fin = true
                      ; opcode =  Pong
                      ; payload_data });
          flush cout
        | Pong -> ()
      end;
      if fin then buf := ""
    done
  with
    Exit -> ()

let send opcode cout bytes =
  output_bytes cout
    (to_bytes { fin = true
              ; opcode
              ; payload_data = bytes });
  flush cout

let send_text = send Text
let send_binary = send Binary

let handshake handler (client_sock, _) =
  let cin = in_channel_of_descr client_sock
  and cout = out_channel_of_descr client_sock in
  begin
    try
      let response = Handshake.(parse_request cin |> make_response) in
      output_bytes cout response;
      flush cout;
      Misc.try_finalize handler client_sock close client_sock
    with
      Handshake.Bad_request response ->
      output_bytes cout response;
      flush cout
  end;
  close client_sock

let handle_error f x = try f x with Failure err -> prerr_endline err; exit 2

let run ?(max_connection=10) ~addr ~port handler =
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
  let srv =
    Misc.tcp_farm_server max_connection treat_connection (ADDR_INET (addr, port))
  in
  handle_unix_error (handle_error srv) ()

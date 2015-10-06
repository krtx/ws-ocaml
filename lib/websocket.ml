open Unix
open Frame

type client = out_channel
type message = Text of bytes | Binary of bytes

let output_bytes' cout bytes =
  try
    output_bytes cout bytes;
    flush cout;
    true
  with _ -> false

let send opcode cout bytes =
  { fin = true; opcode; payload_data = bytes }
  |> to_bytes
  |> output_bytes' cout

let send_text = send Text
let send_binary = send Binary

type app =
  { body          : client -> unit
  ; on_connection : client -> unit
  ; on_message    : client -> message -> unit
  ; on_close      : client -> unit
  }

(* TODO: Handling of Invalid Data (10.7)  *)

let event_loop on_message client_sock =
  let cin = in_channel_of_descr client_sock
  and cout = out_channel_of_descr client_sock in
  let buf = ref (Bytes.create 0) in
  let proc { fin; opcode; payload_data } =
    if opcode = Continuation || opcode = Text || opcode = Binary
    then buf := Bytes.cat !buf payload_data;
    begin
      match opcode with
      | Continuation -> ()
      | Text         -> if fin then on_message cout (Text !buf)
      | Binary       -> if fin then on_message cout (Binary !buf)
      | Close        ->
        (* TODO: Handling of closing status *)
        ignore (send Close cout (Bytes.sub payload_data 0 2)); raise Exit
      | Ping         ->
        if not (send Pong cout payload_data) then raise Exit
      | Pong         -> ()
    end;
    if fin then buf := ""
  in
  try
    while true do
      match read_frame cin with
      | Some frame -> proc frame
      | None       -> raise Exit
    done
  with
    Exit -> ()

let invoke app client_sock =
  let cout = out_channel_of_descr client_sock in
  Misc.try_finalize
    (fun () ->
       app.on_connection cout;
       ignore (Thread.create app.body cout);
       event_loop app.on_message client_sock)
    ()
    app.on_close
    cout

let handshake app (client_sock, _) =
  let cin  = in_channel_of_descr client_sock
  and cout = out_channel_of_descr client_sock in
  begin
    try
      let response = Handshake.(parse_request cin |> make_response) in
      ignore (output_bytes' cout response);
      Misc.try_finalize (invoke app) client_sock close client_sock
    with
      Handshake.Bad_request response ->
      ignore (output_bytes' cout response)
  end;
  try close client_sock with _ -> ()

let handle_error f x = try f x with Failure err -> prerr_endline err; exit 2

let make_app
      ?(body          = (fun _ -> ()))
      ?(on_connection = (fun _ -> ()))
      ?(on_message    = (fun _ _ -> ()))
      ?(on_close      = (fun _ -> ()))
      () =
  { body; on_connection; on_message; on_close }

let run ?(max_connection=10) ~addr ~port app =
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
    Misc.co_treatment (allow_connection_errors (handshake app))
  in
  let srv =
    Misc.tcp_farm_server max_connection treat_connection (ADDR_INET (addr, port))
  in
  handle_unix_error (handle_error srv) ()

open Unix

let input_chars ?(buf_size=1024) cin =
  let buf = ref (Bytes.create buf_size) in
  let rec loop i =
    if Bytes.length !buf <= i then buf := Bytes.extend !buf 0 buf_size;
    let c = input_char cin in
    Bytes.set !buf i c;
    loop (i + 1)
  in
  try loop 0 with End_of_file -> !buf

let input_line_rn ?(buf_size=1024) cin =
  let buf = ref (Bytes.create buf_size) in
  let rec loop i prev =
    if Bytes.length !buf <= i then buf := Bytes.extend !buf 0 buf_size;
    let c = input_char cin in
    if prev = '\r' && c = '\n'
    then Bytes.sub !buf 0 (i - 1)
    else (Bytes.set !buf i c; loop (i + 1) c)
  in
  loop 0 '\000'

let split ~sep s =
  let idx = Bytes.index s sep in
  Bytes.sub s 0 idx, Bytes.sub s (idx + 1) (Bytes.length s - idx - 1)

type request =
  { request_line : bytes
  ; headers      : (bytes, bytes) Hashtbl.t
  ; message_body : bytes
  }

let parse_request cin : request =
  let request_line = input_line_rn cin in

  let tbl = Hashtbl.create 20 in
  let rec headers () =
    let l = try input_line_rn ~buf_size:256 cin with End_of_file -> "" in
    if Bytes.length l > 0
    then begin
      let k, v = try split ~sep:':' l with Not_found -> l, "" in
      let k = k |> Bytes.trim |> Bytes.uppercase
      and v = v |> Bytes.trim
      in
      Hashtbl.add tbl k v;
      headers ()
    end
  in
  headers ();

  let message_body =
    try
      let content_length = Hashtbl.find tbl "CONTENT-LENGTH" |> int_of_string in
      let buf = Bytes.create content_length in
      for i = 0 to content_length - 1 do
        Bytes.set buf i (input_char cin)
      done;
      buf
    with
      Not_found -> ""
  in

  { request_line; headers = tbl; message_body }

exception Bad_request of string
type ('a, 'b) result = Ok of 'a | Error of 'b

let generate_sec_websocket_accept key =
  key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  |> Sha1.string
  |> Sha1.to_bin
  |> B64.encode

let find_eq tbl k v = try Hashtbl.find tbl k = v with Not_found -> false

let handshake_response { request_line; headers; _ } =
  ignore request_line;
  if find_eq headers "UPGRADE" "websocket" && find_eq headers "CONNECTION" "Upgrade"
  then
    try
      let sec_websocket_key     = Hashtbl.find headers "SEC-WEBSOCKET-KEY"
      and sec_websocket_version = Hashtbl.find headers "SEC-WEBSOCKET-VERSION"
      in
      if sec_websocket_version <> "13"
      then
        Error ([ "HTTP/1.1 400 Bad Request"
               ; "Content-Length: 33"
               ; ""
               ; "Unsupported Sec-WebSocket-Version"
               ] |> String.concat "\r\n")
      else
        let accept = generate_sec_websocket_accept sec_websocket_key in
        Ok ([ "HTTP/1.1 101 Switching Protocols"
            ; "Upgrade: websocket"
            ; "Connection: Upgrade"
            ; "Sec-WebSocket-Accept: " ^ accept
            ; ""
            ; ""
            ] |> String.concat "\r\n")
    with
      Not_found -> Error ([ "HTTP/1.1 400 Bad Request"
                          ; "Content-Length: 27"
                          ; ""
                          ; "Required fields are missing"
                          ] |> String.concat "\r\n")
  else
    Error ([ "HTTP/1.1 400 Bad Request"
           ; "Content-Length: 23"
           ; ""
           ; "Not WebSocket handshake"
           ] |> String.concat "\r\n")

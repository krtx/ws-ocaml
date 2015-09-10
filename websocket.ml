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

(* --- test code --- *)

let in_channel_of_bytes b =
  let fn = Filename.temp_file "in_channel_of_string" "" in
  let cout = open_out fn in
  output_bytes cout b;
  flush cout;
  close_out cout;
  open_in fn

let test () =
  let { request_line; headers; message_body } =
    "GET / HTTP/1.1\r\nHost: localhost:3000\r\nConnection: Upgrade\r\nPragma: no-cache\r\nCache-Control: no-cache\r\nUpgrade: websocket\r\nOrigin: null\r\nSec-WebSocket-Version: 13\r\nUser-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.85 Safari/537.36\r\nAccept-Encoding: gzip, deflate, sdch\r\nAccept-Language: ja,en-US;q=0.8,en;q=0.6\r\nSec-WebSocket-Key: eUksYQMw3z+ZMjb6baawiw==\r\nSec-WebSocket-Extensions: permessage-deflate; client_max_window_bits\r\n\r\n"
    |> in_channel_of_bytes
    |> parse_request
  in
  assert (request_line = "GET / HTTP/1.1");
  assert (Hashtbl.find headers "HOST" = "localhost:3000");
  assert (Hashtbl.find headers "CONNECTION" = "Upgrade");
  assert (Hashtbl.find headers "PRAGMA" = "no-cache");
  assert (Hashtbl.find headers "CACHE-CONTROL" = "no-cache");
  assert (Hashtbl.find headers "UPGRADE" = "websocket");
  assert (Hashtbl.find headers "ORIGIN" = "null");
  assert (Hashtbl.find headers "SEC-WEBSOCKET-VERSION" = "13");
  assert (Hashtbl.find headers "USER-AGENT" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.85 Safari/537.36");
  assert (Hashtbl.find headers "ACCEPT-ENCODING" = "gzip, deflate, sdch");
  assert (Hashtbl.find headers "ACCEPT-LANGUAGE" = "ja,en-US;q=0.8,en;q=0.6");
  assert (Hashtbl.find headers "SEC-WEBSOCKET-KEY" = "eUksYQMw3z+ZMjb6baawiw==");
  assert (Hashtbl.find headers "SEC-WEBSOCKET-EXTENSIONS" = "permessage-deflate; client_max_window_bits");
  assert (message_body = "");

  let { request_line; headers; message_body } =
    "POST / HTTP/1.1\r\nHost: localhost:3000\r\nUser-Agent: curl/7.43.0\r\nAccept: */*\r\nContent-Length: 21\r\nContent-Type: application/x-www-form-urlencoded\r\n\r\nquery=hoge fuga hello"
    |> in_channel_of_bytes
    |> parse_request
  in
  assert (request_line = "POST / HTTP/1.1");
  assert (Hashtbl.find headers "HOST" = "localhost:3000");
  assert (Hashtbl.find headers "USER-AGENT" = "curl/7.43.0");
  assert (Hashtbl.find headers "ACCEPT" = "*/*");
  assert (Hashtbl.find headers "CONTENT-LENGTH" = "21");
  assert (Hashtbl.find headers "CONTENT-TYPE" = "application/x-www-form-urlencoded");
  assert (message_body = "query=hoge fuga hello")

open Handshake
open Test_helper

let test () =
  print_endline "Test for Websocket";
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

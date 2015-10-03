open Websocket

module Clients = struct
  type t = client list ref

  let make () = ref []

  let broadcast store message =
    List.iter (fun client -> ignore (Websocket.send_text client message)) !store

  let add store client = store := client :: !store

  let remove store client =
    let rec remove acc = function
      | [] -> acc
      | x :: xs ->
        if x = client
        then remove acc xs
        else remove (x :: acc) xs
    in
    store := remove [] !store
end

let store = Clients.make ()

let on_connection client =
  print_endline "a new client connected";
  Clients.add store client

let on_message client = function
  | Text message -> Clients.broadcast store message
  | Binary _     -> ()

let on_close client =
  print_endline "a client disconnected";
  Clients.remove store client

let () =
  run
    ~addr:"127.0.0.1"
    ~port:"3000"
    (make_app ~on_connection ~on_message ~on_close ())

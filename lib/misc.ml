(* Excerpt from Unix system programming in OCaml *)

open Unix

let rec restart_on_EINTR f x =
  try f x with Unix_error (EINTR, _, _) -> restart_on_EINTR f x

let try_finalize f x finally y =
  let res = try f x with exn -> finally y; raise exn in
  finally y;
  res

let run_with_lock l f x =
  Mutex.lock l; try_finalize f x Mutex.unlock l

let install_tcp_server_socket ?(max_request=100) addr =
  let s = socket PF_INET SOCK_STREAM 0 in
  try
    bind s addr;
    listen s max_request;
    s
  with
    exn -> close s; raise exn

let tcp_farm_server n treat_connection addr =
  let server_sock = install_tcp_server_socket addr in
  let mutex = Mutex.create () in
  let rec serve () =
    let client = run_with_lock mutex (restart_on_EINTR accept) server_sock in
    treat_connection server_sock client;
    serve ()
  in
  for _ = 1 to n - 1 do ignore (Thread.create serve ()) done;
  serve ()

let co_treatment service ((client_descr, _) as client) =
  try ignore (Thread.create service client)
  with exn -> close client_descr; raise exn

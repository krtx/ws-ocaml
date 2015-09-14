open Unix

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
  let addr = !addr
  and port = !port in
  let on_message client = let open App in
    function
    | Text b -> send_text client b
    | Binary b -> send_binary client b
  in
  App.(run ~addr ~port (make ~on_message))

let () =
  [ Frame_test.test
  ; Websocket_test.test
  ]
  |> List.iter (fun f -> f ());
  print_endline "ok"

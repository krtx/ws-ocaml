let () =
  Random.self_init ();
  [ Bytes_ext_test.test
  ; Frame_test.test
  ; Crypto_test.test
  ; Handshake_test.test
  ]
  |> List.iter (fun f -> f ());
  print_endline "ok"

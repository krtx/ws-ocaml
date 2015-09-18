open Bytes_ext

let test_f n =
  let x = Random.bits () land (1 lsl ((n lsl 3) - 1) - 1) in
  assert (x = (bytes_of_int x n |> int_of_bytes))

let test () =
  print_endline "Test for Bytes_ext";
  print_endline "∀ x, int_of_bytes (bytes_of_int x n) = x where n = 1..8";
  for i = 1 to 8 do
    for _ = 0 to 99 do test_f i done
  done

open Crypto

let hex_of_bytes bytes =
  let hex_of_int i =
    if i <= 9
    then int_of_char '0' + i |> char_of_int
    else if i <= 16
    then int_of_char 'a' + i - 10 |> char_of_int
    else failwith "hex_of_int"
  in
  let buf = Bytes.create (Bytes.length bytes * 2) in
  for i = 0 to Bytes.length bytes - 1 do
    let x = Bytes.get bytes i |> int_of_char in
    Bytes.set buf (i * 2) ((x lsr 4) land 15 |> hex_of_int);
    Bytes.set buf (i * 2 + 1) (x land 15 |> hex_of_int)
  done;
  buf

let assert_equal real expect =
  if real <> expect
  then (Printf.printf "\"%s\" expected, but got \"%s\"\n" expect real;
        failwith "assertion failed")

let test () =
  print_endline "Test for Crypto";

  assert_equal (base64encode "") "";

  assert_equal (base64encode "ABCDE") "QUJDREU=";

  assert_equal (base64encode "test for crypto") "dGVzdCBmb3IgY3J5cHRv";

  assert_equal (base64encode "あいうえお") "44GC44GE44GG44GI44GK";

  assert_equal (sha1 "" |> hex_of_bytes) "da39a3ee5e6b4b0d3255bfef95601890afd80709";

  assert_equal
    (sha1 "The quick brown fox jumps over the lazy dog" |> hex_of_bytes)
    "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12";

  assert_equal
    (sha1 "The quick brown fox jumps over the lazy cog" |> hex_of_bytes)
    "de9f2c7fd25e1b3afad3e85a0bd17d9b100db4b3";

  assert_equal
    (sha1 "+" |> hex_of_bytes)
    "a979ef10cc6f6a36df6b8a323307ee3bb2e2db9c";

  assert_equal
    (sha1 "a+=" |> hex_of_bytes)
    "4fbf6df71bd51610c1d3d571bf0bdc688d6ba2ff";

  assert_equal
    (sha1 "WuBsc0JTR+sbvxG4XZuJVw==" |> hex_of_bytes)
    "4c500f1a4a84331fa787d3bd5372dcbb62f829ed";

  assert_equal
    (sha1 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    |> hex_of_bytes)
    "7f9000257a4918d7072655ea468540cdcbd42e0c";

  assert_equal
    (sha1 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"
     |> hex_of_bytes)
    "abac55d2c32213ea7b2f4258bff6a30d832508b3";

  assert_equal
    (sha1 "baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
     |> hex_of_bytes)
    "3c8d0fb02cae247efaf44046c72847cd0f8196e8";

  assert_equal
    (sha1 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
     |> hex_of_bytes)
    "f475597b627a4d580ec1619a94c7afb9cc75abe4";

  assert_equal
    (sha1 "595ffbc156f0b40f004fedb30a267a42b56cacf2c77c8b6012c7da1a7e9d8c97d55332b7ded89ca17bb3f82d7eb3ef1af50791c35bb34356b131ed6dad7fcd2d21606c0cc2014a1775c615f1f1a659ebb086586017cc8f65f696d1a6bd2a5b8506eb6a5e"
     |> hex_of_bytes)
    "a88c5fd7511e4a17b3e86d970daf4f3e1d5f9e4e";

  assert_equal
    (sha1 ("WuBsc0JTR+sbvxG4XZuJVw==" ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")
     |> hex_of_bytes)
    "ed7e382f6d0f43e2b17ed8e1d1dafc7fd3eab7c7";

  assert_equal
    ("c6b8hTg4EeGb2gQMztV1/g==" ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
     |> sha1
     |> base64encode)
    "Kxep+hNu9n51529fGidYu7a3wO0=";

  assert_equal
    ("WuBsc0JTR+sbvxG4XZuJVw==" ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
     |> sha1
     |> base64encode)
    "7X44L20PQ+Kxftjh0dr8f9Pqt8c="

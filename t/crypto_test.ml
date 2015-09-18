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

let test () =
  print_endline "Test for Crypto";
  assert (base64encode "" = "");
  assert (base64encode "ABCDE" = "QUJDREU=");
  assert (base64encode "test for crypto" = "dGVzdCBmb3IgY3J5cHRv");
  assert (base64encode "あいうえお" = "44GC44GE44GG44GI44GK");
  assert ((sha1 "" |> hex_of_bytes) = "da39a3ee5e6b4b0d3255bfef95601890afd80709");
  assert ((sha1 "The quick brown fox jumps over the lazy dog" |> hex_of_bytes)
          = "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12");
  assert ((sha1 "The quick brown fox jumps over the lazy cog" |> hex_of_bytes)
          = "de9f2c7fd25e1b3afad3e85a0bd17d9b100db4b3")

let encode_int x dst start len =
  for i = 0 to len - 1 do
    Bytes.set
      dst
      (i + start)
      ((x lsr ((len - i - 1) lsl 3)) land 0xFF |> char_of_int)
  done

let bytes_of_int x n =
  let res = Bytes.create n in
  encode_int x res 0 n;
  res

let decode_int src start len =
  let acc = ref 0 in
  for i = 0 to len - 1 do
    acc := (!acc lsl 8) lor (Bytes.get src (i + start) |> int_of_char)
  done;
  !acc

let int_of_bytes bytes = decode_int bytes 0 (Bytes.length bytes)

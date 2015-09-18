(* convert function for 6 bits integer *)
let convert x =
  if x <= 25
  then int_of_char 'A' + x |> char_of_int
  else if x <= 51
  then int_of_char 'a' + x - 26 |> char_of_int
  else if x <= 61
  then int_of_char '0' + x - 52 |> char_of_int
  else if x = 62
  then '+'
  else if x = 63
  then '/'
  else failwith (Printf.sprintf "convert: %d" x)

let base64encode bytes =
  let chars   = (Bytes.length bytes * 8 + 5) / 6 in
  let padding = (4 - (chars mod 4)) mod 4 in
  let res     = Bytes.create (chars + padding) in
  let geti i = Bytes.get bytes i |> int_of_char in
  let rec replace idx i j =
    if idx < chars then
      let nextc = if i + 1 < Bytes.length bytes then geti (i + 1) else 0 in
      let c, next_i, next_j =
        match j with
        | 0 -> geti i lsr 2,                                    i, 6
        | 6 -> ((geti i land 0x3) lsl 4) lor (nextc lsr 4), i + 1, 4
        | 4 -> ((geti i land 0xf) lsl 2) lor (nextc lsr 6), i + 1, 2
        | 2 -> geti i land 0o77,                            i + 1, 0
        | _ -> failwith ("replace: invalid j " ^ (string_of_int j))
      in
      Bytes.set res idx (convert c);
      replace (idx + 1) next_i next_j
  in
  replace 0 0 0;
  Bytes.fill res chars padding '='; res

let leftrotate32 x shift =
  ((x lsl shift) land 0xffffffff) lor ((x lsr (32 - shift)) land 0xffffffff)

let sha1 message =
  let len  = Bytes.length message in
  let len' = (((len + 8) lsr 6) + 1) lsl 6 in
  let message =
    let buf = Bytes.make len' '\000' in
    Bytes.blit message 0 buf 0 len;
    Bytes.set  buf len '\x80';
    Bytes_ext.encode_int (len * 8) buf (len' - 8) 8;
    buf
  in
  let h0 = ref 0x67452301
  and h1 = ref 0xEFCDAB89
  and h2 = ref 0x98BADCFE
  and h3 = ref 0x10325476
  and h4 = ref 0xC3D2E1F0
  in
  for i = 0 to len' / 64 - 1 do
    let w = Array.make 80 0 in
    for j = 0 to 15 do
      w.(j) <-
        List.fold_left (fun acc v -> acc lor v) 0
          [ (Bytes.get message (i * 64 + j * 4 + 0) |> int_of_char) lsl 24
          ; (Bytes.get message (i * 64 + j * 4 + 1) |> int_of_char) lsl 16
          ; (Bytes.get message (i * 64 + j * 4 + 2) |> int_of_char) lsl  8
          ; (Bytes.get message (i * 64 + j * 4 + 3) |> int_of_char) lsl  0
          ]
    done;
    for j = 16 to 79 do
      w.(j) <- leftrotate32 (w.(j - 3) lxor w.(j - 8) lxor w.(j - 14) lxor w.(j - 16)) 1
    done;
    let a = ref !h0
    and b = ref !h1
    and c = ref !h2
    and d = ref !h3
    and e = ref !h4
    in
    for j = 0 to 79 do
      let f, k =
        if j <= 19
        then (!b land !c) lor (((lnot !b) land 0xffffffff) land !d), 0x5A827999
        else if j <= 39
        then !b lxor !c lxor !d, 0x6ED9EBA1
        else if j <= 59
        then (!b land !c) lor (!b land !d) lor (!c land !d), 0x8F1BBCDC
        else !b lxor !c lxor !d, 0xCA62C1D6
      in
      let temp = (leftrotate32 !a 5 + f + !e + k + w.(j)) land 0xffffffff in
      e := !d;
      d := !c;
      c := leftrotate32 !b 30;
      b := !a;
      a := temp
    done;
    h0 := !h0 + !a;
    h1 := !h1 + !b;
    h2 := !h2 + !c;
    h3 := !h3 + !d;
    h4 := !h4 + !e
  done;
  let res = Bytes.create 20 in
  Bytes_ext.encode_int !h0 res  0 4;
  Bytes_ext.encode_int !h1 res  4 4;
  Bytes_ext.encode_int !h2 res  8 4;
  Bytes_ext.encode_int !h3 res 12 4;
  Bytes_ext.encode_int !h4 res 16 4;
  res

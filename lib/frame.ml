type t =
  { fin          : bool
  ; opcode       : opcode
  ; payload_data : bytes
  }

and opcode =
  | Continuation
  | Text
  | Binary
  | Close
  | Ping
  | Pong

let int_of_opcode = function
  | Continuation -> 0
  | Text         -> 1
  | Binary       -> 2
  | Close        -> 8
  | Ping         -> 9
  | Pong         -> 10

let opcode_of_int = function
  | 0  -> Continuation
  | 1  -> Text
  | 2  -> Binary
  | 8  -> Close
  | 9  -> Ping
  | 10 -> Pong
  | x  -> failwith ("opcode_of_int: " ^ (string_of_int x))

let mask_of ~key transformed_data =
  let res = Bytes.copy transformed_data in
  let key = Array.init 4 (fun i -> Bytes.get key i |> int_of_char) in
  for i = 0 to Bytes.length transformed_data - 1 do
    let c =
      (Bytes.get transformed_data i |> int_of_char) lxor
      Array.get key (i mod 4)
    in
    Bytes.set res i (c |> char_of_int)
  done;
  res

let to_bytes ?masking_key frame =
  let payload_length = Bytes.length frame.payload_data in
  let frame_size =
    1 +
    (if payload_length <= 125 then 1
     else if payload_length <= 65535 then 3
     else 9) +
    (match masking_key with Some _ -> 4 | None -> 0) +
    payload_length
  in
  let buf = Bytes.create frame_size in
  Bytes.set buf 0 (
    ((if frame.fin then 1 else 0) lsl 7) lor (int_of_opcode frame.opcode)
    |> char_of_int
  );
  let mask = match masking_key with
    | Some _ -> 1 lsl 7
    | None   -> 0
  in
  let pos = ref 0 in
  if payload_length <= 125
  then begin
    Bytes.set buf 1 (mask lor payload_length |> char_of_int);
    pos := 2
  end
  else if payload_length <= 65535
  then begin
    Bytes.set buf 1 (mask lor 126 |> char_of_int);
    Bytes_ext.encode_int payload_length buf 2 2;
    pos := 4
  end
  else begin
    Bytes.set buf 1 (mask lor 127 |> char_of_int);
    Bytes_ext.encode_int payload_length buf 2 8;
    pos := 10
  end;
  begin
    match masking_key with
    | Some key ->
      let key = Bytes_ext.bytes_of_int key 4 in
      Bytes.blit key 0 buf !pos 4;
      Bytes.blit (mask_of ~key frame.payload_data) 0 buf (!pos + 4) payload_length
    | None     ->
      Bytes.blit frame.payload_data 0 buf !pos payload_length
  end;
  buf

let read_frame cin =
  let read_frame cin =
    let buf = ref (Bytes.create 128) in
    let read_bytes offs len =
      if Bytes.length !buf < offs + len
      then buf := Bytes.extend !buf 0 (offs + len);
      for i = offs to offs + len - 1 do
        Bytes.set !buf i (input_char cin)
      done;
      Bytes.sub !buf offs len
    in
    ignore (read_bytes 0 2);
    let fin    = (Bytes.get !buf 0 |> int_of_char) lsr 7 = 1 in
    let opcode = (Bytes.get !buf 0 |> int_of_char) land 15 |> opcode_of_int in
    let mask   = (Bytes.get !buf 1 |> int_of_char) lsr 7 = 1 in
    let pl     = (Bytes.get !buf 1 |> int_of_char) land 127 in
    let payload_length, pos =
      match pl with
      | 126 -> read_bytes 2 2 |> Bytes_ext.int_of_bytes, 4
      | 127 -> read_bytes 2 8 |> Bytes_ext.int_of_bytes, 10
      | _   -> pl, 2
    in
    let payload_data =
      if mask
      then
        let key = read_bytes pos 4
        and raw = read_bytes (pos + 4) payload_length in
        mask_of ~key raw
      else
        read_bytes pos payload_length
    in
    { fin; opcode; payload_data }
  in
  try Some (read_frame cin) with _ -> None

let of_bytes bytes =
  let fin    = (Bytes.get bytes 0 |> int_of_char) lsr 7 = 1 in
  let opcode =
    (Bytes.get bytes 0 |> int_of_char) land 15
    |> opcode_of_int
  in
  let mask   = (Bytes.get bytes 1 |> int_of_char) lsr 7 = 1 in
  let pl     = (Bytes.get bytes 1 |> int_of_char) land 127 in
  let payload_length, pos =
    match pl with
    | 126 -> Bytes.sub bytes 2 2 |> Bytes_ext.int_of_bytes, 4
    | 127 -> Bytes.sub bytes 2 8 |> Bytes_ext.int_of_bytes, 10
    | _   -> pl, 2
  in
  let payload_data =
    if mask
    then
      let key = Bytes.sub bytes pos 4
      and raw = Bytes.sub bytes (pos + 4) payload_length in
      mask_of ~key raw
    else
      Bytes.sub bytes pos payload_length
  in
  { fin; opcode; payload_data }


type t =
  { fin          : bool
  ; opcode       : opcode
  ; payload_data : bytes
  }

and opcode =
  | Continuation
  | Text
  | Binary
  | Connection
  | Ping
  | Pong

let int_of_opcode = function
  | Continuation -> 0
  | Text         -> 1
  | Binary       -> 2
  | Connection   -> 8
  | Ping         -> 9
  | Pong         -> 10

let opcode_of_int = function
  | 0  -> Continuation
  | 1  -> Text
  | 2  -> Binary
  | 8  -> Connection
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

(* encode x into n bytes in big endian *)
let bytes_of_int n x =
  let res = Bytes.create n in
  let rec copy i =
    if i < n then begin
      Bytes.set res i ((x lsr ((n - i - 1) * 8)) land 255 |> char_of_int);
      copy (i + 1)
    end
  in copy 0;
  res

let bytes_of_halfword   x = bytes_of_int 2 x
let bytes_of_word       x = bytes_of_int 4 x
let bytes_of_doubleword x = bytes_of_int 8 x

let int_of_bytes bytes =
  let n = Bytes.length bytes in
  let rec loop i acc =
    if i < n
    then
      loop (i + 1) ((Bytes.get bytes i |> int_of_char) lor (acc lsl 8))
    else
      acc
  in
  loop 0 0

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
    Bytes.blit (payload_length |> bytes_of_halfword) 0 buf 2 2;
    pos := 4
  end
  else begin
    Bytes.set buf 1 (mask lor 127 |> char_of_int);
    Bytes.blit (payload_length |> bytes_of_doubleword) 0 buf 2 8;
    pos := 10
  end;
  begin
    match masking_key with
    | Some key ->
      let key = key |> bytes_of_word in
      Bytes.blit key 0 buf !pos 4;
      Bytes.blit (mask_of ~key frame.payload_data) 0 buf (!pos + 4) payload_length
    | None     ->
      Bytes.blit frame.payload_data 0 buf !pos payload_length
  end;
  buf

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
    | 126 -> Bytes.sub bytes 2 2 |> int_of_bytes, 4
    | 127 -> Bytes.sub bytes 2 8 |> int_of_bytes, 10
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

(* --- test code --- *)

let test1 x =
  let x' = x |> bytes_of_halfword |> int_of_bytes in
  if x <> x'
  then begin
    Printf.eprintf "check1: %d expected, but got %d\n" x x';
    assert false
  end

let test2 x =
  let x' = x |> bytes_of_doubleword |> int_of_bytes in
  if x <> x'
  then begin
    Printf.eprintf "check2: %d expected, but got %d\n" x x';
    assert false
  end

let test3 ({ fin; opcode; payload_data } as f) =
  let { fin          = fin'
      ; opcode       = opcode'
      ; payload_data = payload_data' } = to_bytes f |> of_bytes in
  assert (fin = fin');
  assert (opcode = opcode');
  assert (payload_data = payload_data')

let test4 ({ fin; opcode; payload_data } as f) =
  let { fin          = fin'
      ; opcode       = opcode'
      ; payload_data = payload_data' } =
    to_bytes ~masking_key:(Random.int (1 lsl 30 - 1)) f |> of_bytes in
  assert (fin = fin');
  assert (opcode = opcode');
  assert (payload_data = payload_data')

let test () =
  Random.self_init ();
  print_endline "::: ∀ x, int_of_bytes (bytes_of_halfword x) = x :::";
  for _ = 1 to 100 do
    (fun () -> test1 (Random.int 65536)) ()
  done;
  print_endline "ok";
  print_endline "::: ∀ x, int_of_bytes (bytes_of_double_word x) = x :::";
  for _ = 1 to 100 do
    (fun () -> test2 (Random.int64 (Int64.shift_left 1L 62) |> Int64.to_int)) ()
  done;
  print_endline "ok";
  print_endline "::: ∀ x, of_bytes (to_bytes x) = x :::";
  test3 { fin          = false
        ; opcode       = Continuation
        ; payload_data = Bytes.of_string "hello"
        };
  test3 { fin          = false
        ; opcode       = Text
        ; payload_data = Bytes.of_string "hello hello hello"
        };
  test3 { fin          = true
        ; opcode       = Connection
        ; payload_data = Bytes.of_string "\r\n\000"
        };
  test3 { fin          = true
        ; opcode       = Binary
        ; payload_data = Bytes.create 125
        };
  test3 { fin          = true
        ; opcode       = Binary
        ; payload_data = Bytes.create 200
        };
  test3 { fin          = false
        ; opcode       = Binary
        ; payload_data = Bytes.create 20000
        };
  test3 { fin          = false
        ; opcode       = Text
        ; payload_data = Bytes.create 65535
        };
  test3 { fin          = false
        ; opcode       = Text
        ; payload_data = Bytes.create 65536
        };
  test3 { fin          = false
        ; opcode       = Binary
        ; payload_data = Bytes.create 123456789
        };
  test3 { fin          = false
        ; opcode       = Binary
        ; payload_data = Bytes.create 300000000
        };
  print_endline "ok";
  print_endline "::: ∀ x, of_bytes (to_bytes with a ranodm key x) = x :::";
  test4 { fin          = false
        ; opcode       = Continuation
        ; payload_data = Bytes.of_string "hello"
        };
  test4 { fin          = false
        ; opcode       = Text
        ; payload_data = Bytes.of_string "hello hello hello"
        };
  test4 { fin          = true
        ; opcode       = Connection
        ; payload_data = Bytes.of_string "\r\n\000"
        };
  test4 { fin          = true
        ; opcode       = Binary
        ; payload_data = Bytes.create 125
        };
  test4 { fin          = true
        ; opcode       = Binary
        ; payload_data = Bytes.create 200
        };
  test4 { fin          = false
        ; opcode       = Binary
        ; payload_data = Bytes.create 20000
        };
  test4 { fin          = false
        ; opcode       = Text
        ; payload_data = Bytes.create 65535
        };
  test4 { fin          = false
        ; opcode       = Text
        ; payload_data = Bytes.create 65536
        };
  test4 { fin          = false
        ; opcode       = Binary
        ; payload_data = Bytes.create 3456789
        };
  test4 { fin          = false
        ; opcode       = Binary
        ; payload_data = Bytes.create 30000000
        };
  print_endline "ok"


(* Invoke tests when the program name ends with 'frame.byte' or 'frame.native' *)
let () =
  let prog = Sys.argv.(0) in
  let l = String.length prog in
  if (l >= 10 && String.sub prog (l - 10) 10 = "frame.byte") ||
     (l >= 12 && String.sub prog (l - 12) 12 = "frame.native")
  then test ()

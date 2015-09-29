open Frame
open Test_helper

let test_frames =
  [ { fin          = false
    ; opcode       = Continuation
    ; payload_data = Bytes.of_string "hello"
    }
  ; { fin          = false
    ; opcode       = Text
    ; payload_data = Bytes.of_string "hello hello hello"
    }
  ; { fin          = true
    ; opcode       = Close
    ; payload_data = Bytes.of_string "\r\n\000"
    }
  ; { fin          = true
    ; opcode       = Binary
    ; payload_data = Bytes.create 125
    }
  ; { fin          = true
    ; opcode       = Binary
    ; payload_data = Bytes.create 200
    }
  ; { fin          = false
    ; opcode       = Binary
    ; payload_data = Bytes.create 20000
    }
  ; { fin          = false
    ; opcode       = Text
    ; payload_data = Bytes.create 65535
    }
  ; { fin          = false
    ; opcode       = Text
    ; payload_data = Bytes.create 65536
    }
  ; { fin          = false
    ; opcode       = Binary
    ; payload_data = Bytes.create 1234567
    }
  ; { fin          = false
    ; opcode       = Binary
    ; payload_data = Bytes.create 3000000
    }
  ]

let value = function
  | Some v -> v
  | None   -> failwith "value"

let test_f mask ({ fin; opcode; payload_data } as f) =
  let { fin          = fin'
      ; opcode       = opcode'
      ; payload_data = payload_data' } =
    (if mask
     then to_bytes ~masking_key:(Random.int (1 lsl 30 - 1)) f
     else to_bytes f)
    |> in_channel_of_bytes
    |> read_frame
    |> value
  in
  assert (fin          = fin');
  assert (opcode       = opcode');
  assert (payload_data = payload_data')

let test () =
  print_endline "Test for Frame";
  print_endline "∀ x, read (to_bytes x) = x";
  List.iter (test_f false) test_frames;
  print_endline "∀ x, read (to_bytes with a random key x) = x";
  List.iter (test_f true) test_frames

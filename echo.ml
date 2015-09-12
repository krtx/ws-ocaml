open Unix
open Frame

let app client_sock =
  let cin = in_channel_of_descr client_sock
  and cout = out_channel_of_descr client_sock in
  let buf = ref (Bytes.create 0) in
  try
    while true do
      let { fin; opcode; payload_data } = read_frame cin in
      begin
        match opcode with
        | Close ->
          output_bytes cout
            (to_bytes { fin = true
                      ; opcode = Close
                      ; payload_data = Bytes.sub payload_data 0 2 });
          flush cout;
          raise Exit
        | Ping  ->
          output_bytes cout
            (to_bytes { fin = true
                      ; opcode = Pong
                      ; payload_data });
          flush cout
        | Pong  -> ()
        | Continuation | Text | Binary -> ()
      end;
      buf := Bytes.cat !buf payload_data;
      if fin then begin
        output_bytes cout
          (to_bytes { fin = true
                    ; opcode = Text
                    ; payload_data = !buf });
        flush cout;
        buf := ""
      end
    done
  with
    Exit -> ()

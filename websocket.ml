open Unix

let input_line_rn ?(buf_size=512) cin =
  let buf = ref (Bytes.create buf_size) in
  let rec loop i prev =
    if Bytes.length !buf <= i then buf := Bytes.extend !buf 0 buf_size;
    let c = input_char cin in
    if prev = '\r' && c = '\n'
    then Bytes.sub !buf 0 (i - 1)
    else (Bytes.set !buf i c; loop (i + 1) c)
  in
  loop 0 '\000'

let split ~sep s =
  let idx = Bytes.index s sep in
  Bytes.sub s 0 idx, Bytes.sub s (idx + 1) (Bytes.length s - idx - 1)


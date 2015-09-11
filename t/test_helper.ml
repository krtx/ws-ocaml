let in_channel_of_bytes b =
  let fn = Filename.temp_file "in_channel_of_string" "" in
  let cout = open_out fn in
  output_bytes cout b;
  flush cout;
  close_out cout;
  open_in fn

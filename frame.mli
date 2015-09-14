(** WebSocket data frame module.  Users of this library are not supposed to
    use this module. *)

(** The type of data frames.  rsv fileds are simply ignored. *)
type t =
  { fin          : bool
  ; opcode       : opcode
  ; payload_data : bytes
  }

(** The type of opcode. *)
and opcode =
  | Continuation
  | Text
  | Binary
  | Close
  | Ping
  | Pong

(** Encode a frame.  If masking_key is given the data frame is masked, but the
    server don't need to mask frames. *)
val to_bytes   : ?masking_key:int -> t -> bytes

(** Decode a frame. *)
val of_bytes   : bytes -> t

(** Read a frame.  Masked frames are unmasked internally. *)
val read_frame : in_channel -> t

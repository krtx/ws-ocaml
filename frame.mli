(* rsv fields are simply ignored *)
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

val to_bytes : ?masking_key:int -> t -> bytes
val of_bytes : bytes -> t

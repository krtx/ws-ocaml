type t
type client
type message = Text of bytes | Binary of bytes

val make : on_message:(client -> message -> unit) -> t
val send : Frame.opcode -> client -> bytes -> unit
val send_text : client -> bytes -> unit
val send_binary : client -> bytes -> unit
val run : ?max_connection:int -> addr:string -> port:string -> t -> unit

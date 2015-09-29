(** WebSocket application module. *)

(** The type of applications. *)
type t

(** The type of clients. *)
type client

(** The type of message from client. *)
type message = Text of bytes | Binary of bytes

(** Make websocket application. *)
val make : on_message:(client -> message -> unit) -> body:(client -> unit) -> t

(** Low-level send function.  Return false when failed. *)
val send : Frame.opcode -> client -> bytes -> bool

(** Send text data to a client.  Return false when failed. *)
val send_text : client -> bytes -> bool

(** Send binary data to a client.  Return false when failed. *)
val send_binary : client -> bytes -> bool

(** Run an application in addr and port.  Default max_connection is 10. *)
val run : ?max_connection:int -> addr:string -> port:string -> t -> unit

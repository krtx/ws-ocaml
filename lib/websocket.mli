(** WebSocket application module. *)

(** The type of applications. *)
type t

(** The type of clients. *)
type client

(** The type of message from client. *)
type message = Text of bytes | Binary of bytes

(** Make websocket application. *)
val make : on_message:(client -> message -> unit) -> t

(** Low-level send function. *)
val send : Frame.opcode -> client -> bytes -> unit

(** Send text data to a client. *)
val send_text : client -> bytes -> unit

(** Send binary data to a client. *)
val send_binary : client -> bytes -> unit

(** Run an application in addr and port.  Default max_connection is 10. *)
val run : ?max_connection:int -> addr:string -> port:string -> t -> unit

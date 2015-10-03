(** WebSocket application module. *)

(** The type of clients. *)
type client

(** The type of message from client. *)
type message = Text of bytes | Binary of bytes

(** The type of applications. *)
type app

(** Low-level send function.  Return false when failed. *)
val send : Frame.opcode -> client -> bytes -> bool

(** Send text data to a client.  Return false when failed. *)
val send_text : client -> bytes -> bool

(** Send binary data to a client.  Return false when failed. *)
val send_binary : client -> bytes -> bool

(** Make a websocket application.  Default function of each argument
    simply ignore the events. *)
val make_app :
  ?body:(client -> unit)
  -> ?on_connection:(client -> unit)
  -> ?on_message:(client -> message -> unit)
  -> ?on_close:(client -> unit)
  -> unit -> app

(** Run an application in addr and port.  Default max_connection is 10. *)
val run : ?max_connection:int -> addr:string -> port:string -> app -> unit

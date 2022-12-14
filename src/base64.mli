(** [hex_decode] takes a hex-encoding string and decodes it *)
val hex_decode : string -> string

(** [hex_encode] takes a plaintext string and hex-encodes it *)
val hex_encode : string -> string

(** [b64encode] encodes an input string in base64 *)
val b64encode : string -> string

(** [b64decode] decodes a base64 input string *)
val b64decode : string -> (string, Base.Error.t) Result.t

val b64char : string -> (char, Base.Error.t) Result.t
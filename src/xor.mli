(** [xor] takes two hex-encoded strings; outputs the XOR of the two strings. *)
val xor : string -> string -> string

(** [xor_encode] takes plaintext and a key; ouputs a cipher string *)
val xor_encode : string -> char -> string

(** [xor_decipher] attempts to decipher a cipher string without the key *)
val xor_decipher : string -> string

(** [xor_decipher_from_list] returns the best match for a deciphered string
    from the given list of cipher strings *)
val xor_decipher_from_list : string list -> string
(** [xor] takes two hex-encoded strings; outputs the XOR of the two strings. *)
val xor : string -> string -> string

(** [xor_encode] takes plaintext and a key; ouputs a cipher string *)
val xor_encode : string -> char -> string

(** [xor_repeating_key_encode] encodes plaintext input with a repeating key *)
val xor_repeating_key_encode : string -> string -> string

(** [xor_decipher] attempts to decipher a cipher string without the key *)
val xor_decipher : string -> string

(** [xor_decipher_from_list] returns the best match for a deciphered string
    from the given list of cipher strings *)
val xor_decipher_from_list : string list -> string

(** [hamming_distance] computes the Hamming distance between two strings, i.e.
    the number of differing bits.

    If the strings are unequal length returns an [Error.t], otherwise returns
    an [int].*)
val hamming_distance : string -> string -> (int, Base.Error.t) Result.t

(** [set_bit_count] returns the number of bits that are 1 in the binary
    representation of an integer *)
val set_bit_count : int -> int

val split_to_blocks : 'a list -> int -> 'a list list

(** [score_split] given ciphertext and the a key length returns a normalized
    "score", with lower scores being a more likely key length for a repeating
    key cipher *)
val score_split : string -> int -> (float, Base.Error.t) Result.t
type t [@@deriving sexp]
type norm_t [@@deriving sexp]

val empty : t
val touch : t -> char -> t

val normalize : t -> norm_t

(** Compares the frequency with English language character frequencies. A score
    is returned, smaller scores are closer to English text. *)
val compare_with_english : t -> float
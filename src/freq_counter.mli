type t [@@deriving sexp]
type norm_t [@@deriving sexp]

val empty : t
val touch : t -> char -> t

val normalize : t -> norm_t


val compare_with_english : t -> float
(** [discard_partial] takes a list assumed to consist of sublists of equal
    lengths, with the possible exception of the last sublist.  If the last
    sublist is not equal length to the others it is discarded. *)
val discard_partial: 'a list list -> 'a list list
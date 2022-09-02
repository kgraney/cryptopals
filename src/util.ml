open! Core

let discard_partial splits =
  match List.hd splits, List.last splits with
  | Some hd, Some tl ->
    if phys_equal (List.length hd) (List.length tl)
    then splits
    else List.sub splits ~pos:0 ~len:(List.length splits - 1)
  | _, _ -> splits
;;

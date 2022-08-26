open! Core

type t = int Char.Caseless.Map.t [@@deriving sexp]
type norm_t = float Char.Caseless.Map.t [@@deriving sexp]

let empty = Char.Caseless.Map.empty

let touch counts line =
  let update v =
    match v with
    | None -> 1
    | Some c -> c + 1
  in
  Char.Caseless.Map.update counts line ~f:update
;;

let normalize counts =
  let accumulator ~key:_ ~data:v accum = v + accum in
  let denom = Char.Caseless.Map.fold ~init:0 ~f:accumulator counts in
  Char.Caseless.Map.map counts ~f:(fun x -> float_of_int x /. float_of_int denom)
;;

(* English character frequencies.
   Source: http://www.macfreek.nl/memory/Letter_Distribution *)
let english =
  Char.Caseless.Map.of_alist_exn
    [ ' ', 1828846265
    ; 'E', 1026665037
    ; 'T', 751699827
    ; 'A', 653216702
    ; 'O', 615957725
    ; 'N', 571201113
    ; 'I', 566844326
    ; 'S', 531700534
    ; 'R', 498790855
    ; 'H', 497856396
    ; 'L', 331754796
    ; 'D', 328292310
    ; 'U', 227579536
    ; 'C', 223367596
    ; 'M', 202656783
    ; 'F', 198306716
    ; 'W', 170389377
    ; 'G', 162490441
    ; 'P', 150432428
    ; 'Y', 142766662
    ; 'B', 125888074
    ; 'V', 079611644
    ; 'K', 056096272
    ; 'X', 014092016
    ; 'J', 009752181
    ; 'Q', 008367550
    ; 'Z', 005128469
    ]
  |> normalize
;;

(* TODOlet%test "english frequencies are normalized" = List.fold ~init:0 ~f:(+) *)

let compare_with_english counts =
  let accumulator ~key:_ ~data:v accum = v +. accum in
  let normalized = normalize counts in
  let merge ~key:_ v =
    Some
      (match v with
       | `Left unknown -> 10.0 *. unknown
       | `Right unused -> 10.0 *. unused
       | `Both (here, english) -> english -. here |> Float.abs)
  in
  (*let () = 
    Debug.eprints "foo" (Char.Caseless.Map.merge normalized english ~f:merge) sexp_of_norm_t
  in*)
  Char.Caseless.Map.merge normalized english ~f:merge
  |> Char.Caseless.Map.fold ~init:0.0 ~f:accumulator
;;

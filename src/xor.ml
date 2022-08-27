open! Core

let xor lhs rhs =
  let lhs_bytes = Base64.hex_decode lhs in
  let rhs_bytes = Base64.hex_decode rhs in
  let zipped, _ = List.zip_with_remainder lhs_bytes rhs_bytes in
  List.map ~f:(fun (x, y) -> Int.bit_xor x y) zipped |> Base64.hex_encode
;;

let string_of_charlist l = l |> List.map ~f:(String.make 1) |> String.concat ~sep:""

let xor_decode cipher key =
  let data = Base64.hex_decode cipher in
  List.map ~f:(Fn.compose Char.of_int_exn (Int.bit_xor key)) data |> string_of_charlist
;;

let xor_encode plaintext key =
  let data = String.to_list plaintext in
  List.map ~f:(Fn.compose (Int.bit_xor (Char.to_int key)) Char.to_int) data
  |> Base64.hex_encode
;;

let xor_score cipher key =
  let data = Base64.hex_decode cipher in
  List.map ~f:(Fn.compose Char.of_int_exn (Int.bit_xor key)) data
  |> List.fold ~init:Freq_counter.empty ~f:Freq_counter.touch
  |> Freq_counter.compare_with_english
;;

let xor_decipher_with_scores cipher =
  let scores =
    List.map ~f:(fun x -> xor_score cipher x, x) (List.range 0x00 0xff ~stop:`inclusive)
  in
  List.sort scores ~compare:Poly.compare
  |> List.hd_exn
  |> fun (score, key) -> score, xor_decode cipher key
;;

let xor_decipher = Fn.compose snd xor_decipher_with_scores

let xor_decipher_from_list ciphers =
  List.map ~f:xor_decipher_with_scores ciphers
  |> List.sort ~compare:Poly.compare
  |> List.hd_exn
  |> snd
;;
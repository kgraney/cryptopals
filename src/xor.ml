open! Core

let string_of_charlist l = l |> List.map ~f:(String.make 1) |> String.concat ~sep:""
let intlist_of_string s = s |> String.to_list |> List.map ~f:Char.to_int

let xor lhs rhs =
  let lhs_bytes = lhs |> String.to_list in
  let rhs_bytes = rhs |> String.to_list in
  let zipped, _ = List.zip_with_remainder lhs_bytes rhs_bytes in
  List.map ~f:(fun (x, y) -> Int.bit_xor (Char.to_int x) (Char.to_int y)) zipped
  |> List.map ~f:Char.of_int_exn
  |> string_of_charlist
;;

let xor_decode_chars cipher key =
  let open List.Let_syntax in
  let%map data = String.to_list cipher in
  Int.bit_xor key (Char.to_int data) |> Char.of_int_exn
;;

let xor_decode cipher key = xor_decode_chars cipher key |> string_of_charlist

let xor_encode plaintext key =
  let lst =
    let open List.Let_syntax in
    let%map data = intlist_of_string plaintext in
    Int.bit_xor (Char.to_int key) data |> Char.of_int_exn
  in
  String.of_char_list lst
;;

let xor_repeating_key_encode ~key plaintext =
  let full_key = intlist_of_string key in
  let rec encode pt ky accum =
    let zipped, rest = List.zip_with_remainder pt ky in
    let new_accum = List.append accum zipped in
    match rest with
    | Some (First word) -> encode word full_key new_accum
    | Some (Second _) -> new_accum
    | None -> new_accum
  in
  encode (intlist_of_string plaintext) full_key []
  |> List.map ~f:(fun (x, y) -> Int.bit_xor x y)
  |> List.map ~f:Char.of_int_exn
  |> String.of_char_list
;;

let xor_score cipher key =
  xor_decode_chars cipher key
  |> List.fold ~init:Freq_counter.empty ~f:Freq_counter.touch
  |> Freq_counter.compare_with_english
;;

let xor_decipher_with_score cipher =
  let scores =
    List.map ~f:(fun x -> xor_score cipher x, x) (List.range 0x00 0xff ~stop:`inclusive)
  in
  List.sort scores ~compare:Poly.compare
  |> List.hd_exn
  |> fun (score, key) -> score, (Char.of_int_exn key), xor_decode cipher key
;;

let xor_decipher_with_key =
  Fn.compose (fun (_, key, deciphered) -> (key, deciphered)) xor_decipher_with_score
;;

let xor_decipher = Fn.compose (fun (_, _, deciphered) -> deciphered) xor_decipher_with_score

let set_bit_count n =
  let rec recurse n count =
    match n with
    | 0 -> count
    | _ -> recurse (n lsr 1) (count + (n land 0x1))
  in
  recurse n 0
;;

let hamming_distance s1 s2 =
  let xor_and_count (x, y) = x lxor y |> set_bit_count in
  let zipped, remainder =
    List.zip_with_remainder (intlist_of_string s1) (intlist_of_string s2)
  in
  match remainder with
  | None -> Ok (zipped |> List.map ~f:xor_and_count |> List.fold_right ~f:( + ) ~init:0)
  | Some _ ->
    Error (Error.of_string "hamming_distance: input strings are not of equal length")
;;

let split_to_blocks list blocksize =
  let rec recurse list accum =
    let block, rest = List.split_n list blocksize in
    match List.length rest with
    | 0 -> List.append accum [ block ]
    | _ -> recurse rest (List.append accum [ block ])
  in
  recurse list []
;;

let score_split ciphertext blocksize =
  let splits = split_to_blocks (String.to_list ciphertext) blocksize in
  let args = List.nth splits 0, List.nth splits 1 in
  match args with
  | Some x, Some y ->
    let open Result.Let_syntax in
    let%map dist = hamming_distance (string_of_charlist x) (string_of_charlist y) in
    float_of_int dist /. float_of_int blocksize
  | _, _ -> Error (Error.of_string "score_split: ciphertext is too short!")
;;

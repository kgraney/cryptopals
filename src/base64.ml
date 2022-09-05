open! Core

exception Internal_errors of Error.t list

(* Convert a 6-bit binary string to a base64 character according to RFC 4648 §4.
   https://datatracker.ietf.org/doc/html/rfc4648#section-4
   *)
let b64char bits =
  match bits with
  | "000000" -> Ok 'A'
  | "000001" -> Ok 'B'
  | "000010" -> Ok 'C'
  | "000011" -> Ok 'D'
  | "000100" -> Ok 'E'
  | "000101" -> Ok 'F'
  | "000110" -> Ok 'G'
  | "000111" -> Ok 'H'
  | "001000" -> Ok 'I'
  | "001001" -> Ok 'J'
  | "001010" -> Ok 'K'
  | "001011" -> Ok 'L'
  | "001100" -> Ok 'M'
  | "001101" -> Ok 'N'
  | "001110" -> Ok 'O'
  | "001111" -> Ok 'P'
  | "010000" -> Ok 'Q'
  | "010001" -> Ok 'R'
  | "010010" -> Ok 'S'
  | "010011" -> Ok 'T'
  | "010100" -> Ok 'U'
  | "010101" -> Ok 'V'
  | "010110" -> Ok 'W'
  | "010111" -> Ok 'X'
  | "011000" -> Ok 'Y'
  | "011001" -> Ok 'Z'
  | "011010" -> Ok 'a'
  | "011011" -> Ok 'b'
  | "011100" -> Ok 'c'
  | "011101" -> Ok 'd'
  | "011110" -> Ok 'e'
  | "011111" -> Ok 'f'
  | "100000" -> Ok 'g'
  | "100001" -> Ok 'h'
  | "100010" -> Ok 'i'
  | "100011" -> Ok 'j'
  | "100100" -> Ok 'k'
  | "100101" -> Ok 'l'
  | "100110" -> Ok 'm'
  | "100111" -> Ok 'n'
  | "101000" -> Ok 'o'
  | "101001" -> Ok 'p'
  | "101010" -> Ok 'q'
  | "101011" -> Ok 'r'
  | "101100" -> Ok 's'
  | "101101" -> Ok 't'
  | "101110" -> Ok 'u'
  | "101111" -> Ok 'v'
  | "110000" -> Ok 'w'
  | "110001" -> Ok 'x'
  | "110010" -> Ok 'y'
  | "110011" -> Ok 'z'
  | "110100" -> Ok '0'
  | "110101" -> Ok '1'
  | "110110" -> Ok '2'
  | "110111" -> Ok '3'
  | "111000" -> Ok '4'
  | "111001" -> Ok '5'
  | "111010" -> Ok '6'
  | "111011" -> Ok '7'
  | "111100" -> Ok '8'
  | "111101" -> Ok '9'
  | "111110" -> Ok '+'
  | "111111" -> Ok '/'
  | b -> Error (Error.t_of_sexp [%message "Invalid base64 bit string" ~bitstring:b])
;;

let inverseb64char bits =
  match bits with
  | 'A' -> Ok "000000"
  | 'B' -> Ok "000001"
  | 'C' -> Ok "000010"
  | 'D' -> Ok "000011"
  | 'E' -> Ok "000100"
  | 'F' -> Ok "000101"
  | 'G' -> Ok "000110"
  | 'H' -> Ok "000111"
  | 'I' -> Ok "001000"
  | 'J' -> Ok "001001"
  | 'K' -> Ok "001010"
  | 'L' -> Ok "001011"
  | 'M' -> Ok "001100"
  | 'N' -> Ok "001101"
  | 'O' -> Ok "001110"
  | 'P' -> Ok "001111"
  | 'Q' -> Ok "010000"
  | 'R' -> Ok "010001"
  | 'S' -> Ok "010010"
  | 'T' -> Ok "010011"
  | 'U' -> Ok "010100"
  | 'V' -> Ok "010101"
  | 'W' -> Ok "010110"
  | 'X' -> Ok "010111"
  | 'Y' -> Ok "011000"
  | 'Z' -> Ok "011001"
  | 'a' -> Ok "011010"
  | 'b' -> Ok "011011"
  | 'c' -> Ok "011100"
  | 'd' -> Ok "011101"
  | 'e' -> Ok "011110"
  | 'f' -> Ok "011111"
  | 'g' -> Ok "100000"
  | 'h' -> Ok "100001"
  | 'i' -> Ok "100010"
  | 'j' -> Ok "100011"
  | 'k' -> Ok "100100"
  | 'l' -> Ok "100101"
  | 'm' -> Ok "100110"
  | 'n' -> Ok "100111"
  | 'o' -> Ok "101000"
  | 'p' -> Ok "101001"
  | 'q' -> Ok "101010"
  | 'r' -> Ok "101011"
  | 's' -> Ok "101100"
  | 't' -> Ok "101101"
  | 'u' -> Ok "101110"
  | 'v' -> Ok "101111"
  | 'w' -> Ok "110000"
  | 'x' -> Ok "110001"
  | 'y' -> Ok "110010"
  | 'z' -> Ok "110011"
  | '0' -> Ok "110100"
  | '1' -> Ok "110101"
  | '2' -> Ok "110110"
  | '3' -> Ok "110111"
  | '4' -> Ok "111000"
  | '5' -> Ok "111001"
  | '6' -> Ok "111010"
  | '7' -> Ok "111011"
  | '8' -> Ok "111100"
  | '9' -> Ok "111101"
  | '+' -> Ok "111110"
  | '/' -> Ok "111111"
  | c -> Error (Error.t_of_sexp [%message "Invalid base64 character" ~char:(c : char)])
;;

let hex_decode s =
  let rec recurse xs =
    if phys_equal (String.length xs) 0
    then []
    else
      Char.of_int_exn (int_of_string ("0x" ^ String.sub xs ~pos:0 ~len:2))
      :: recurse (String.sub xs ~pos:2 ~len:(String.length xs - 2))
  in
  recurse s |> String.of_char_list
;;

let hex_encode s =
  let rec recurse ints =
    match ints with
    | [] -> ""
    | x :: xs -> Printf.sprintf "%02x" x ^ recurse xs
  in
  String.to_list s |> List.map ~f:Char.to_int |> recurse
;;

let rec base2_string i =
  if i < 2 then string_of_int i else base2_string (i / 2) ^ string_of_int (i % 2)
;;

let pad_eight_bits x =
  let pad_len = max 0 (8 - String.length x) in
  let padding = String.make pad_len '0' in
  padding ^ x
;;

(** If [pad] is false discards the remaining bits. *)
let rec split_bits ~pad n xs =
  let pad_or_discard pad_len =
    match pad with
    | true ->
      let padding = String.make pad_len '0' in
      split_bits ~pad n (xs ^ padding)
    | false -> []
  in
  let pad_len = n - String.length xs in
  match String.is_empty xs, Ordering.of_int pad_len with
  | true, _ -> []
  | false, Greater -> pad_or_discard pad_len
  | false, _ ->
    String.sub xs ~pos:0 ~len:n
    :: split_bits ~pad n (String.sub xs ~pos:n ~len:(String.length xs - n))
;;

(*
  if phys_equal (String.length xs) 0
  then []
  else if pad_len > 0
  then
    if pad
    then (
      let padding = String.make pad_len '0' in
      split_bits ~pad:pad n (xs ^ padding))
    else []
  else
    String.sub xs ~pos:0 ~len:n
    :: split_bits ~pad:pad n (String.sub xs ~pos:n ~len:(String.length xs - n))
    *)

let base2_to_base64 = split_bits ~pad:true 6
let base2_to_base256 = split_bits ~pad:false 8

let bits_to_ascii bitstring =
  let chars =
    let open List.Let_syntax in
    let%map bitseq = bitstring |> base2_to_base256 in
    int_of_string ("0b" ^ bitseq) |> Char.of_int_exn
  in
  String.of_char_list chars
;;

let pad_b64_output s =
  let pad_len =
    match 4 - (String.length s mod 4) with
    | 4 -> 0
    | x -> x
  in
  s ^ String.make pad_len '='
;;

let b64encode s =
  let open List.Let_syntax in
  let ints = List.map ~f:Char.to_int (String.to_list s) in
  let strings =
    let%map chars =
      let%map bytestring =
        List.map ~f:(Fn.compose pad_eight_bits base2_string) ints
        |> String.concat ~sep:""
        |> base2_to_base64
      in
      b64char bytestring
    in
    let open Result.Let_syntax in
    let%bind c = chars in
    Ok (String.make 1 c)
  in
  match Result.combine_errors strings with
  | Ok lst -> (String.concat ~sep:"" lst |> pad_b64_output)
  | Error errors -> raise (Internal_errors errors)
;;

let b64decode s =
  let chars =
    let open List.Let_syntax in
    let%map c = String.to_list s |> List.filter ~f:(fun c -> Char.( <> ) c '=') in
    inverseb64char c
  in
  match Result.combine_errors chars with
  | Ok lst -> Ok (String.concat ~sep:"" lst |> bits_to_ascii)
  | Error e ->
    Error
      (Error.t_of_sexp
         [%message "Invalid base64 string" ~b64:s ~error:(e : Error.t list)])
;;

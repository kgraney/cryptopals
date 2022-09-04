open! Core

(* Convert a 6-bit binary string to a base64 character according to RFC 4648 §4.
   https://datatracker.ietf.org/doc/html/rfc4648#section-4
   *)
let b64char bits =
  match bits with
  | "000000" -> 'A'
  | "000001" -> 'B'
  | "000010" -> 'C'
  | "000011" -> 'D'
  | "000100" -> 'E'
  | "000101" -> 'F'
  | "000110" -> 'G'
  | "000111" -> 'H'
  | "001000" -> 'I'
  | "001001" -> 'J'
  | "001010" -> 'K'
  | "001011" -> 'L'
  | "001100" -> 'M'
  | "001101" -> 'N'
  | "001110" -> 'O'
  | "001111" -> 'P'
  | "010000" -> 'Q'
  | "010001" -> 'R'
  | "010010" -> 'S'
  | "010011" -> 'T'
  | "010100" -> 'U'
  | "010101" -> 'V'
  | "010110" -> 'W'
  | "010111" -> 'X'
  | "011000" -> 'Y'
  | "011001" -> 'Z'
  | "011010" -> 'a'
  | "011011" -> 'b'
  | "011100" -> 'c'
  | "011101" -> 'd'
  | "011110" -> 'e'
  | "011111" -> 'f'
  | "100000" -> 'g'
  | "100001" -> 'h'
  | "100010" -> 'i'
  | "100011" -> 'j'
  | "100100" -> 'k'
  | "100101" -> 'l'
  | "100110" -> 'm'
  | "100111" -> 'n'
  | "101000" -> 'o'
  | "101001" -> 'p'
  | "101010" -> 'q'
  | "101011" -> 'r'
  | "101100" -> 's'
  | "101101" -> 't'
  | "101110" -> 'u'
  | "101111" -> 'v'
  | "110000" -> 'w'
  | "110001" -> 'x'
  | "110010" -> 'y'
  | "110011" -> 'z'
  | "110100" -> '0'
  | "110101" -> '1'
  | "110110" -> '2'
  | "110111" -> '3'
  | "111000" -> '4'
  | "111001" -> '5'
  | "111010" -> '6'
  | "111011" -> '7'
  | "111100" -> '8'
  | "111101" -> '9'
  | "111110" -> '+'
  | "111111" -> '/'
  | b -> raise_s [%message "Invalid bit string" b]
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

let hex_decode_int xs =
  let open List.Let_syntax in
  let%map c = hex_decode xs |> String.to_list in
  Char.to_int c
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

let rec base2_to_base64 xs =
  let pad_len = 6 - String.length xs in
  if phys_equal (String.length xs) 0
  then []
  else if pad_len > 0
  then (
    let padding = String.make pad_len '0' in
    base2_to_base64 (xs ^ padding))
  else
    String.sub xs ~pos:0 ~len:6
    :: base2_to_base64 (String.sub xs ~pos:6 ~len:(String.length xs - 6))
;;

let pad_b64_output s =
  let pad_len =
    match 4 - (String.length s mod 4) with
    | 4 -> 0
    | x -> x
  in
  s ^ String.make pad_len '='
;;

let b64encode h =
  let decoded = hex_decode_int h in
  let padded_bytes = List.map ~f:(Fn.compose pad_eight_bits base2_string) decoded in
  String.concat ~sep:"" padded_bytes
  |> base2_to_base64
  |> List.map ~f:(Fn.compose (String.make 1) b64char)
  |> String.concat ~sep:""
  |> pad_b64_output
;;

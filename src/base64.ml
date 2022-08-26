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

let%expect_test "b64char A" =
  Out_channel.output_char stdout (b64char "000000");
  [%expect {| A |}]
;;

let%expect_test "b64char r" =
  Out_channel.output_char stdout (b64char "101011");
  [%expect {| r |}]
;;

let%test "b64char wrong" = Exn.does_raise (fun () -> b64char "111")

let rec hex_decode xs =
  if phys_equal (String.length xs) 0
  then []
  else
    int_of_string ("0x" ^ String.sub xs ~pos:0 ~len:2)
    :: hex_decode (String.sub xs ~pos:2 ~len:(String.length xs - 2))
;;

let rec hex_encode ints =
  match ints with
  | [] -> ""
  | x :: xs -> Printf.sprintf "%02x" x ^ hex_encode xs
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
  let decoded = hex_decode h in
  let padded_bytes = List.map ~f:(Fn.compose pad_eight_bits base2_string) decoded in
  String.concat ~sep:"" padded_bytes
  |> base2_to_base64
  |> List.map ~f:(Fn.compose (String.make 1) b64char)
  |> String.concat ~sep:""
  |> pad_b64_output
;;

let%expect_test "b64encode cryptopals example" =
  print_string
    (b64encode
       "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d");
  [%expect {| SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t |}]
;;

let%expect_test "b64encode odd size" =
  print_string (b64encode "49276d206b");
  [%expect {| SSdtIGs= |}]
;;

let%expect_test "hexdecode basic" =
  [%sexp_of: int list] (hex_decode "49276d206b") |> Sexp.to_string |> print_string;
  [%expect {| (73 39 109 32 107) |}]
;;

let%expect_test "hexdecode empty" =
  [%sexp_of: int list] (hex_decode "") |> Sexp.to_string |> print_string;
  [%expect {| () |}]
;;

let%test "hexdecode invalid" = Exn.does_raise (fun () -> hex_decode "zz")

let%expect_test "hexencode basic" =
  hex_encode [ 73; 39; 109; 32; 107 ] |> print_string;
  [%expect {| 49276d206b |}]
;;

let%expect_test "hexencode empty" =
  hex_encode [] |> print_string;
  [%expect {| |}]
;;

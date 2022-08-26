open! Core

let xor lhs rhs =
  let lhs_bytes = Base64.hex_decode lhs in
  let rhs_bytes = Base64.hex_decode rhs in
  let zipped, _ = List.zip_with_remainder lhs_bytes rhs_bytes in
  List.map ~f:(fun (x, y) -> Int.bit_xor x y) zipped |> Base64.hex_encode
;;

let%expect_test "xor basic" =
  xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"
  |> print_string;
  [%expect {| 746865206b696420646f6e277420706c6179 |}]
;;

let string_of_charlist l = l |> List.map ~f:(String.make 1) |> String.concat ~sep:""

let xor_decode cipher key =
  let data = Base64.hex_decode cipher in
  List.map ~f:(Fn.compose Char.of_int_exn (Int.bit_xor key)) data |> string_of_charlist
;;

let xor_encode plaintext key =
  let data = String.to_list plaintext in
  List.map ~f:(Fn.compose (Int.bit_xor key) Char.to_int) data |> Base64.hex_encode
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

let%expect_test "xor_decipher example" =
  xor_decipher "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  |> print_string;
  [%expect {| Cooking MC's like a pound of bacon |}]
;;

let%expect_test "xor_decipher others" =
  xor_encode "How now brown cow." 20 |> xor_decipher |> print_endline;
  xor_encode "Don't wait for the show to start!" 20 |> xor_decipher |> print_endline;
  xor_encode "How do you do?  I'm great!" 20 |> xor_decipher |> print_endline;
  [%expect
    {|
    How now brown cow.
    Don't wait for the show to start!
    How do you do?  I'm great! |}]
;;

let%expect_test "xor_decipher different keys" =
  for key = 0x00 to 0xff do
    xor_encode "How do you do?  I'm great!" key |> xor_decipher |> print_endline
  done;
  [%expect
    {|
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great!
    How do you do?  I'm great! |}]
;;

let%expect_test "xor_file" =
  Stdio.In_channel.read_lines "../../../input/4.txt"
  |> xor_decipher_from_list
  |> print_string;
  [%expect {| Now that the party is jumping |}]
;;

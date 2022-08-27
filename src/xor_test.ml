open! Core
open Xor

let%expect_test "xor basic" =
  xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"
  |> print_string;
  [%expect {| 746865206b696420646f6e277420706c6179 |}]
;;

let%expect_test "xor_decipher example" =
  xor_decipher "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  |> print_string;
  [%expect {| Cooking MC's like a pound of bacon |}]
;;

let%expect_test "xor_decipher others" =
  xor_encode "How now brown cow." 'A' |> xor_decipher |> print_endline;
  xor_encode "Don't wait for the show to start!" 'B' |> xor_decipher |> print_endline;
  xor_encode "How do you do?  I'm great!" 'C' |> xor_decipher |> print_endline;
  [%expect
    {|
    How now brown cow.
    Don't wait for the show to start!
    How do you do?  I'm great! |}]
;;

let%expect_test "xor_decipher different keys" =
  for key = 0x00 to 0xff do
    xor_encode "How do you do?  I'm great!" (Char.of_int_exn key)
    |> xor_decipher
    |> print_endline
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

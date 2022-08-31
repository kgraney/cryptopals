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
  let check key =
    xor_encode "How do you do?  I'm great!" key |> xor_decipher |> print_endline;
    [%expect {| How do you do?  I'm great! |}]
  in
  List.range 0x00 0xff |> List.map ~f:Char.of_int_exn |> List.iter ~f:check
;;

let%expect_test "xor_file" =
  Stdio.In_channel.read_lines "../../../input/4.txt"
  |> xor_decipher_from_list
  |> print_string;
  [%expect {| Now that the party is jumping |}]
;;

let%expect_test "xor_repeating_key_encode" =
  let input =
    "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
  in
  xor_repeating_key_encode input "ICE" |> print_string;
  [%expect
    {| 0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f |}]
;;

let%expect_test "set_bit_count 0" =
  set_bit_count 0 |> printf "%d";
  [%expect {| 0 |}]
;;

let%expect_test "set_bit_count 1" =
  set_bit_count 1 |> printf "%d";
  [%expect {| 1 |}]
;;

let%expect_test "set_bit_count 3" =
  set_bit_count 3 |> printf "%d";
  [%expect {| 2 |}]
;;

let%expect_test "set_bit_count 8" =
  set_bit_count 8 |> printf "%d";
  [%expect {| 1 |}]
;;

let%expect_test "set_bit_count 9" =
  set_bit_count 9 |> printf "%d";
  [%expect {| 2 |}]
;;

let%expect_test "hamming_distance" =
  [%sexp_of: (int, Error.t) Result.t] (hamming_distance "this is a test" "wokka wokka!!!")
  |> Sexp.to_string
  |> print_string;
  [%expect {| (Ok 37) |}]
;;

let%expect_test "hamming_distance unequal length" =
  [%sexp_of: (int, Error.t) Result.t] (hamming_distance "this is a test" "wokka wokka!")
  |> Sexp.to_string
  |> print_string;
  [%expect {| (Error"hamming_distance: input strings are not of equal length") |}]
;;

let%expect_test "split_to_blocks even" =
  [%sexp_of: int list list] (split_to_blocks (List.range 0 12) 3)
  |> Sexp.to_string
  |> print_string;
  [%expect {| ((0 1 2)(3 4 5)(6 7 8)(9 10 11)) |}]
;;

let%expect_test "split_to_blocks uneven" =
  [%sexp_of: int list list] (split_to_blocks (List.range 0 14) 3)
  |> Sexp.to_string
  |> print_string;
  [%expect {| ((0 1 2)(3 4 5)(6 7 8)(9 10 11)(12 13)) |}]
;;

let%expect_test "split_to_blocks uneven" =
  [%sexp_of: int list list] (split_to_blocks (List.range 0 2) 3)
  |> Sexp.to_string
  |> print_string;
  [%expect {| ((0 1)) |}]
;;

let%expect_test "score_split distance" =
  [%sexp_of: (float, Error.t) Result.t] (score_split "aaabbb" 3)
  |> Sexp.to_string
  |> print_string;
  [%expect {| (Ok 2) |}]
;;

let%expect_test "score_split more distance" =
  [%sexp_of: (float, Error.t) Result.t] (score_split "abcdefghijklmnopqrstuv" 8)
  |> Sexp.to_string
  |> print_string;
  [%expect {| (Ok 1.125) |}]
;;

let%expect_test "score_split no distance" =
  [%sexp_of: (float, Error.t) Result.t] (score_split "aaaaaa" 3)
  |> Sexp.to_string
  |> print_string;
  [%expect {| (Ok 0) |}]
;;

let%expect_test "score_split too short" =
  [%sexp_of: (float, Error.t) Result.t] (score_split "aaaaaa" 4)
  |> Sexp.to_string
  |> print_string;
  [%expect {| (Error"hamming_distance: input strings are not of equal length") |}]
;;

let%expect_test "score_split way too short" =
  [%sexp_of: (float, Error.t) Result.t] (score_split "aaaaaa" 6)
  |> Sexp.to_string
  |> print_string;
  [%expect {| (Error"score_split: ciphertext is too short!") |}]
;;

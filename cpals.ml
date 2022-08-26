open! Core
open Cryptopals

let set1_1 () =
  let input =
    "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
  in
  let base64 = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" in
  print_endline "Set 1.1";
  printf "expected: %s\n" base64;
  printf "output:   %s\n" (Base64.b64encode input)
;;

let set1_2 () =
  let lhs = "1c0111001f010100061a024b53535009181c" in
  let rhs = "686974207468652062756c6c277320657965" in
  let expected = "746865206b696420646f6e277420706c6179" in
  print_endline "Set 1.2";
  printf "expected: %s\n" expected;
  printf "output:   %s\n" (Xor.xor lhs rhs)
;;

let set1_3 () =
  print_endline "Set 1.3";
  let input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736" in
  printf "output:   %s\n" (Xor.xor_decipher input)
;;

let set1_4 () =
  print_endline "Set 1.4";
  let soln =
    Stdio.In_channel.read_lines "./input/4.txt" |> Xor.xor_decipher_from_list
  in
  printf "output:   %s\n" soln
;;

let () =
  set1_1 ();
  set1_2 ();
  set1_3 ();
  set1_4 ()
;;

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
  let lhs = Base64.hex_decode "1c0111001f010100061a024b53535009181c" in
  let rhs = Base64.hex_decode "686974207468652062756c6c277320657965" in
  let expected = "746865206b696420646f6e277420706c6179" in
  print_endline "Set 1.2";
  printf "expected: %s\n" expected;
  printf "output:   %s\n" (Base64.hex_encode (Xor.xor lhs rhs))
;;

let set1_3 () =
  print_endline "Set 1.3";
  let input =
    "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    |> Base64.hex_decode
  in
  printf "output:   %s\n" (Xor.xor_decipher input)
;;

let set1_4 () =
  print_endline "Set 1.4";
  let scored =
    let open List.Let_syntax in
    let%map line = Stdio.In_channel.read_lines "./input/4.txt" in
    Base64.hex_decode line |> Xor.xor_decipher_with_score
  in
  List.sort ~compare:Poly.compare scored
  |> List.hd_exn
  |> fun (_, _, s) -> printf "output:   %s\n" s
;;

let set1_5 () =
  print_endline "Set 1.5";
  let input =
    "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
  in
  let soln = Xor.xor_repeating_key_encode ~key:"ICE" input |> Base64.hex_encode in
  printf "output:   %s\n" soln
;;

let set1_6 () =
  print_endline "Set 1.6";
  let input =
    Stdio.In_channel.read_lines "./input/6.txt"
    |> String.concat ~sep:""
    |> Base64.b64decode
    |> Result.ok
    |> Option.value_exn
  in
  match Xor.xor_repeating_key_decipher input with
  | Some (key, soln) ->
    printf "key:   %s\n" key;
    print_string soln
  | None -> print_string "FAILED!"
;;

let () =
  set1_1 ();
  set1_2 ();
  set1_3 ();
  set1_4 ();
  set1_5 ();
  set1_6 ()
;;

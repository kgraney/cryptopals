open! Core
open Xor

let%expect_test "xor basic" =
  xor
    (Base64.hex_decode "1c0111001f010100061a024b53535009181c")
    (Base64.hex_decode "686974207468652062756c6c277320657965")
  |> Base64.hex_encode
  |> print_string;
  [%expect {| 746865206b696420646f6e277420706c6179 |}]
;;

let%expect_test "xor_decipher example" =
  "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  |> Base64.hex_decode
  |> xor_decipher
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
  let scored =
    let open List.Let_syntax in
    let%map line = Stdio.In_channel.read_lines "../../..//input/4.txt" in
    Base64.hex_decode line |> Xor.xor_decipher_with_score
  in
  [%sexp_of: (float * char * string) option]
    (List.sort ~compare:Poly.compare scored |> List.hd)
  |> Sexp.to_string
  |> print_string;
  [%expect {| ((2.1132745578746159 5"Now that the party is jumping\n")) |}]
;;

let%expect_test "xor_repeating_key_encode" =
  let input =
    "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
  in
  xor_repeating_key_encode ~key:"ICE" input |> Base64.hex_encode |> print_string;
  [%expect
    {| 0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f |}]
;;

let%expect_test "xor_repeating_key_encode" =
  let input =
    "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
  in
  xor_repeating_key_encode ~key:"ICE" input
  |> xor_repeating_key_encode ~key:"ICE"
  |> print_string;
  [%expect
    {|
      Burning 'em, if you ain't quick and nimble
      I go crazy when I hear a cymbal |}]
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

let%expect_test "transpose 2" =
  [%sexp_of: string list] (transpose "foobarbaz" 2) |> Sexp.to_string |> print_string;
  [%expect {| (foab obra) |}]
;;

let%expect_test "transpose empty" =
  [%sexp_of: string list] (transpose "" 2) |> Sexp.to_string |> print_string;
  [%expect {| () |}]
;;

let%expect_test "transpose 3" =
  [%sexp_of: string list] (transpose "allthesmallthings" 3)
  |> Sexp.to_string
  |> print_string;
  [%expect {| (atslh lhmli leatn) |}]
;;

let%expect_test "xor_repeating_key_decipher input6" =
  [%sexp_of: (string * string) option]
    (xor_repeating_key_encode
       ~key:"baz"
       "This is a story about something else nothing all the time!"
    |> xor_repeating_key_decipher)
  |> Sexp.to_string
  |> print_string;
  [%expect {| ((baz"This is a story about something else nothing all the time!")) |}]
;;

let input6 =
  Stdio.In_channel.read_lines "../../../input/6.txt"
  |> String.concat ~sep:""
  |> Base64.b64decode
  |> Result.ok
  |> Option.value_exn
;;

let%expect_test "xor_repeating_key_decipher input6" =
  [%sexp_of: (string * string) option] (xor_repeating_key_decipher input6)
  |> Sexp.to_string
  |> print_string;
  [%expect
    {| (("Terminator X: Bring the noise""I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell \nIn ecstasy in the back of me \nWell that's my DJ Deshay cuttin' all them Z's \nHittin' hard and the girlies goin' crazy \nVanilla's on the mike, man I'm not lazy. \n\nI'm lettin' my drug kick in \nIt controls my mouth and I begin \nTo just let it flow, let my concepts go \nMy posse's to the side yellin', Go Vanilla Go! \n\nSmooth 'cause that's the way I will be \nAnd if you don't give a damn, then \nWhy you starin' at me \nSo get off 'cause I control the stage \nThere's no dissin' allowed \nI'm in my own phase \nThe girlies sa y they love me and that is ok \nAnd I can dance better than any kid n' play \n\nStage 2 -- Yea the one ya' wanna listen to \nIt's off my head so let the beat play through \nSo I can funk it up and make it sound good \n1-2-3 Yo -- Knock on some wood \nFor good luck, I like my rhymes atrocious \nSupercalafragilisticexpialidocious \nI'm an effect and that you can bet \nI can take a fly girl and make her wet. \n\nI'm like Samson -- Samson to Delilah \nThere's no denyin', You can try to hang \nBut you'll keep tryin' to get my style \nOver and over, practice makes perfect \nBut not if you're a loafer. \n\nYou'll get nowhere, no place, no time, no girls \nSoon -- Oh my God, homebody, you probably eat \nSpaghetti with a spoon! Come on and say it! \n\nVIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino \nIntoxicating so you stagger like a wino \nSo punks stop trying and girl stop cryin' \nVanilla Ice is sellin' and you people are buyin' \n'Cause why the freaks are jockin' like Crazy Glue \nMovin' and groovin' trying to sing along \nAll through the ghetto groovin' this here song \nNow you're amazed by the VIP posse. \n\nSteppin' so hard like a German Nazi \nStartled by the bases hittin' ground \nThere's no trippin' on mine, I'm just gettin' down \nSparkamatic, I'm hangin' tight like a fanatic \nYou trapped me once and I thought that \nYou might have it \nSo step down and lend me your ear \n'89 in my time! You, '90 is my year. \n\nYou're weakenin' fast, YO! and I can tell it \nYour body's gettin' hot, so, so I can smell it \nSo don't be mad and don't be sad \n'Cause the lyrics belong to ICE, You can call me Dad \nYou're pitchin' a fit, so step back and endure \nLet the witch doctor, Ice, do the dance to cure \nSo come up close and don't be square \nYou wanna battle me -- Anytime, anywhere \n\nYou thought that I was weak, Boy, you're dead wrong \nSo come on, everybody and sing this song \n\nSay -- Play that funky music Say, go white boy, go white boy go \nplay that funky music Go white boy, go white boy, go \nLay down and boogie and play that funky music till you die. \n\nPlay that funky music Come on, Come on, let me hear \nPlay that funky music white boy you say it, say it \nPlay that funky music A little louder now \nPlay that funky music, white boy Come on, Come on, Come on \nPlay that funky music \n")) |}]
;;
open! Core
open Base64

let%expect_test "b64char A" =
  Out_channel.output_char stdout (b64char "000000");
  [%expect {| A |}]
;;

let%expect_test "b64char r" =
  Out_channel.output_char stdout (b64char "101011");
  [%expect {| r |}]
;;

let%test "b64char wrong" = Exn.does_raise (fun () -> b64char "111")

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

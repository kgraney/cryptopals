open! Core
open Base64

let%expect_test "b64char A" =
  [%sexp_of: (char, Error.t) Result.t] (b64char "000000")
  |> Sexp.to_string
  |> print_string;
  [%expect {| (Ok A) |}]
;;

let%expect_test "b64char r" =
  [%sexp_of: (char, Error.t) Result.t] (b64char "101011")
  |> Sexp.to_string
  |> print_string;
  [%expect {| (Ok r) |}]
;;

let%expect_test "b64char wrong" =
  [%sexp_of: (char, Error.t) Result.t] (b64char "111") |> Sexp.to_string |> print_string;
  [%expect {| (Error("Invalid base64 bit string"(bitstring 111))) |}]
;;

let%expect_test "b64encode cryptopals example" =
  "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
  |> hex_decode
  |> b64encode
  |> print_string;
  [%expect {| SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t |}]
;;

let%expect_test "b64encode odd size" =
  "49276d206b" |> hex_decode |> b64encode |> print_string;
  [%expect {| SSdtIGs= |}]
;;

let%expect_test "b64encode odd size" =
  "a" |> b64encode |> print_string;
  [%expect {| YQ== |}]
;;

let%expect_test "b64decode" =
  [%sexp_of: (string, Error.t) Result.t]
    (b64decode "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  |> Sexp.to_string
  |> print_string;
  [%expect {| (Ok"I'm killing your brain like a poisonous mushroom") |}]
;;

let%expect_test "b64decode" =
  [%sexp_of: (string, Error.t) Result.t] (b64decode "SSdtIGs=")
  |> Sexp.to_string
  |> print_string;
  [%expect {| (Ok"I'm k") |}]
;;

let%expect_test "encode/decode" =
  [ "foobar"; "baz"; "I will decode!"; "a" ]
  |> List.iter ~f:(fun x ->
         [%sexp_of: (string, Error.t) Result.t] (b64encode x |> b64decode)
         |> Sexp.to_string
         |> print_string);
  [%expect {| (Ok foobar)(Ok baz)(Ok"I will decode!")(Ok a) |}]
;;

let hex_decode_int x =
  let open List.Let_syntax in
  let%map hex = hex_decode x |> String.to_list in
  Char.to_int hex
;;

let%expect_test "hexdecode basic" =
  [%sexp_of: int list] (hex_decode_int "49276d206b") |> Sexp.to_string |> print_string;
  [%expect {| (73 39 109 32 107) |}]
;;

let%expect_test "hexdecode empty" =
  [%sexp_of: int list] (hex_decode_int "") |> Sexp.to_string |> print_string;
  [%expect {| () |}]
;;

let%test "hexdecode invalid" = Exn.does_raise (fun () -> hex_decode_int "zz")

let%expect_test "hexencode basic" =
  hex_encode "I'm k" |> print_string;
  [%expect {| 49276d206b |}]
;;

let%expect_test "hexencode empty" =
  hex_encode "" |> print_string;
  [%expect {| |}]
;;
open! Core
open Block

let%expect_test "pkcs7_1" =
  pkcs7 "YELLOW SUBMARINE" |> Base64.hex_encode |> print_string;
  [%expect {| 59454c4c4f57205355424d4152494e4510101010101010101010101010101010 |}]

let%expect_test "pkcs7_1" =
  pkcs7 "YELLOW SUBMARINE YELLOW SUBMARINE" |> Base64.hex_encode |> print_string;
  [%expect {| 59454c4c4f57205355424d4152494e452059454c4c4f57205355424d4152494e450f0f0f0f0f0f0f0f0f0f0f0f0f0f0f |}]

let%expect_test "pkcs7_2" =
  pkcs7 "foo" |> Base64.hex_encode |> print_string;
  [%expect {| 666f6f0d0d0d0d0d0d0d0d0d0d0d0d0d |}]

let%expect_test "pkcs7_3" =
  pkcs7 "Hello World" |> Base64.hex_encode |> print_string;
  [%expect {| 48656c6c6f20576f726c640505050505 |}]
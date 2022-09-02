open! Core
open Util

let%expect_test "discard_partial basic discard" =
  [%sexp_of: int list list] (discard_partial [ [ 1; 2 ]; [ 3; 4 ]; [ 5 ] ])
  |> Sexp.to_string
  |> print_string;
  [%expect {| ((1 2)(3 4)) |}]
;;

let%expect_test "discard_partial basic non-discard" =
  [%sexp_of: int list list] (discard_partial [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ] ])
  |> Sexp.to_string
  |> print_string;
  [%expect {| ((1 2)(3 4)(5 6)) |}]
;;

let%expect_test "discard_partial empty" =
  [%sexp_of: int list list] (discard_partial [ [] ]) |> Sexp.to_string |> print_string;
  [%expect {| (()) |}]
;;

let%expect_test "discard_partial single list" =
  [%sexp_of: int list list] (discard_partial [ [ 6; 5; 4 ] ])
  |> Sexp.to_string
  |> print_string;
  [%expect {| ((6 5 4)) |}]
;;

let%expect_test "discard_partial two elements" =
  [%sexp_of: int list list] (discard_partial [ [ 6; 5; 4 ]; [6; 7; 8] ])
  |> Sexp.to_string
  |> print_string;
  [%expect {| ((6 5 4)(6 7 8)) |}]
;;

let%expect_test "discard_partial two elements discard" =
  [%sexp_of: int list list] (discard_partial [ [ 6; 5; 4 ]; [0; 9] ])
  |> Sexp.to_string
  |> print_string;
  [%expect {| ((6 5 4)) |}]
;;

open! Core
module ECB = Nocrypto.Cipher_block.AES.ECB

let keyfor7 = ECB.of_secret (Cstruct.of_string "YELLOW SUBMARINE")

let decrypt7 ciphertext =
  let pt = ECB.decrypt ~key:keyfor7 (Cstruct.of_string ciphertext) in
  Cstruct.to_string pt
;;

(** ECB encryption has blocksize of 16 bytes.  If the same 16 byte block is
    repeated in the input text it will also be repeated in the encrypted
    text. *)
let max_repeated_blocks ciphertext =
  let table = Hashtbl.create (module String) in
  let add_block block =
    String.of_char_list block
    |> Hashtbl.update table ~f:(fun v ->
           match v with
           | None -> 1
           | Some x -> x + 1)
  in
  let blocks = Xor.split_to_blocks (String.to_list ciphertext) 16 in
  (* Debug.eprint_s ([%sexp_of: char list list] blocks); *)
  List.iter blocks ~f:add_block;
  Hashtbl.data table |> List.max_elt ~compare:Int.compare
;;

let%expect_test "max_repeated_blocks" =
  let plaintext = String.make 16 'a' ^ String.make 16 'b' ^ String.make 16 'c' ^ String.make 16 'a' in
  let encrypt x = Cstruct.of_string x |> ECB.encrypt ~key:keyfor7 |> Cstruct.to_string in
  [%sexp_of: int option] (encrypt plaintext |> max_repeated_blocks)
  |> Sexp.to_string
  |> print_string;
  [%expect {||}]
;;

let most_likely_ciphertext hex_ciphertexts =
  let ranked =
    let open List.Let_syntax in
    let%map ciphertext = hex_ciphertexts in
    Base64.hex_decode ciphertext |> max_repeated_blocks, ciphertext
  in
  (* TODO: even if none of the ciphertexts have repeated blocks this will return one of them. *)
  List.sort ~compare:Poly.descending ranked |> List.hd |> Option.map ~f:snd
;;

let%expect_test "max_repeated_blocks" =
  [%sexp_of: string option]
    (Stdio.In_channel.read_lines "../../../input/8.txt" |> most_likely_ciphertext)
  |> Sexp.to_string
  |> print_string;
  [%expect {||}]
;;
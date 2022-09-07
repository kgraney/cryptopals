open! Core
module ECB = Nocrypto.Cipher_block.AES.ECB

let keyfor7 = ECB.of_secret (Cstruct.of_string "YELLOW SUBMARINE")

let decrypt7 ciphertext =
  let pt = ECB.decrypt ~key:keyfor7 (Cstruct.of_string ciphertext) in
  Cstruct.to_string pt
;;


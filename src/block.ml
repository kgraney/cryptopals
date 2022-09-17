open! Core

let pkcs7 text = 
  let m = (String.length text) mod 16 in
  let (pad_size, pad_char) = match m with
  | 0 -> (16, 0x10)
  | 1 -> (15, 0x0f)
  | 2 -> (14, 0x0e)
  | 3 -> (13, 0x0d)
  | 4 -> (12, 0x0c)
  | 5 -> (11, 0x0b)
  | 6 -> (10, 0x0a)
  | 7 -> (9, 0x09)
  | 8 -> (8, 0x08)
  | 9 -> (7, 0x07)
  | 10 -> (6, 0x06)
  | 11 -> (5, 0x05)
  | 12 -> (4, 0x04)
  | 13 -> (3, 0x03)
  | 14 -> (2, 0x02)
  | 15 -> (1, 0x01)
  | _ -> (0, 0x00)
  in
  text ^ (Char.of_int_exn pad_char |> String.make pad_size)
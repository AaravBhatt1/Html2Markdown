open Seq

(* This function takes a seq of characters and gets the first line of it as a list of characters *)
let getFirstLine seq =
  let isNotNewLine c = c != '\n'
  in (take_while isNotNewLine seq |> List.of_seq), (drop_while isNotNewLine seq |> drop 1)

open Seq

(* This function takes a seq of characters and gets the first line of it as a list of characters *)
let getFirstLine seq =
  let isNotNewLine c = c != '\n'
  in (take_while isNotNewLine seq |> List.of_seq), (drop_while isNotNewLine seq |> drop 1)

module MarkdownLineStarter = struct
  type t =
    | Header of int
    | UnorderedListItem

  let to_string = function
    | Header n -> "Header " ^ string_of_int n
    | UnorderedListItem -> "UnorderedListItem"

  let equal a b =
    match a, b with
    | Header n1, Header n2 -> n1 = n2
    | UnorderedListItem, UnorderedListItem -> true
    | _, _ -> false

  let pp fmt v =
    Format.fprintf fmt "%s" (to_string v)
end

type markdownText =
  | NormalText of string
  | BoldText of string
  | ItalicText of string

type markdownLine = MarkdownLine of MarkdownLineStarter.t option * markdownText list

(* This function gets the markdownLineStarter of the line *)
let parseLineStart = function
  | '#' :: xn ->
    let rec countHeader acc = function
      | '#' :: rest -> countHeader (acc + 1) rest
      | rest -> acc, List.tl rest
    in let headerVal, restOfLine = countHeader 1 xn
    in Some (MarkdownLineStarter.Header headerVal), restOfLine
  | '-' :: xn -> Some (MarkdownLineStarter.UnorderedListItem), xn
  | xn -> None, xn

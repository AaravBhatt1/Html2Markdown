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

module MarkdownText = struct
  type t =
    | NormalText of string
    | BoldText of string
    | ItalicText of string

  let to_string = function
    | NormalText s -> "NormalText(" ^ s ^ ")"
    | BoldText s -> "BoldText(" ^ s ^ ")"
    | ItalicText s -> "ItalicText(" ^ s ^ ")"

  let equal a b =
    match a, b with
    | NormalText s1, NormalText s2 -> s1 = s2
    | BoldText s1, BoldText s2 -> s1 = s2
    | ItalicText s1, ItalicText s2 -> s1 = s2
    | _, _ -> false

  let pp fmt v =
    Format.fprintf fmt "%s" (to_string v)
end

type markdownLine = MarkdownLine of MarkdownLineStarter.t option * MarkdownText.t list

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

module TextToken = struct
  type t =
    | RegularTextToken of string
    | BoldToken
    | ItalicToken

  let to_string = function
    | RegularTextToken s -> "RegularTextToken(" ^ s ^ ")"
    | BoldToken -> "BoldToken"
    | ItalicToken -> "ItalicToken"

  let equal a b =
    match a, b with
    | RegularTextToken s1, RegularTextToken s2 -> s1 = s2
    | BoldToken, BoldToken -> true
    | ItalicToken, ItalicToken -> true
    | _, _ -> false

  let pp fmt v =
    Format.fprintf fmt "%s" (to_string v)
end

(* This function should tokenize the text in the line *)
let tokenizeText text =
  let rec tokenize = function
    | '*' :: '*' :: xn -> TextToken.ItalicToken :: tokenize xn
    | '*' :: xn -> TextToken.BoldToken :: tokenize xn
    | x :: xn -> TextToken.RegularTextToken (Char.escaped x) :: tokenize xn
    | [] -> []
  in let rec combine = function
    | TextToken.RegularTextToken a :: TextToken.RegularTextToken b :: xn -> combine (TextToken.RegularTextToken (a ^ b) :: xn)
    | x :: xn -> x :: combine xn
    | [] -> []
  in text |> tokenize |> combine

let rec parseTokens =
  let open TextToken
  in function
    | BoldToken :: RegularTextToken t :: BoldToken :: xn -> MarkdownText.BoldText t :: parseTokens xn
    | ItalicToken :: RegularTextToken t :: ItalicToken :: xn -> MarkdownText.ItalicText t :: parseTokens xn
    | RegularTextToken t :: xn -> MarkdownText.NormalText t :: parseTokens xn
    | [] -> []
    | _ -> failwith "Parsing Error"

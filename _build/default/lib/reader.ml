(* This function takes a seq of characters and gets the first line of it as a list of characters *)
let getFirstLine (seq : char Seq.t) : char list * char Seq.t =
  let isNotNewLine c = c != '\n'
  in (Seq.take_while isNotNewLine seq |> List.of_seq), (Seq.drop_while isNotNewLine seq |> Seq.drop 1)

(* Module for representing the start of a Markdown line *)
module MarkdownLineStarter = struct
  type t =
    | Header of int
    | UnorderedListItem

  (* Convert a MarkdownLineStarter to a string representation *)
  let to_string = function
    | Header n -> "Header " ^ string_of_int n
    | UnorderedListItem -> "UnorderedListItem"

  (* Check equality of two MarkdownLineStarters *)
  let equal a b =
    match a, b with
    | Header n1, Header n2 -> n1 = n2
    | UnorderedListItem, UnorderedListItem -> true
    | _, _ -> false

  (* Pretty-print a MarkdownLineStarter *)
  let pp fmt v =
    Format.fprintf fmt "%s" (to_string v)
end

(* Module for representing different types of text in Markdown *)
module MarkdownText = struct
  type t =
    | NormalText of string
    | BoldText of string
    | ItalicText of string

  (* Convert a MarkdownText to a string representation *)
  let to_string = function
    | NormalText s -> "NormalText(" ^ s ^ ")"
    | BoldText s -> "BoldText(" ^ s ^ ")"
    | ItalicText s -> "ItalicText(" ^ s ^ ")"

  (* Check equality of two MarkdownTexts *)
  let equal a b =
    match a, b with
    | NormalText s1, NormalText s2 -> s1 = s2
    | BoldText s1, BoldText s2 -> s1 = s2
    | ItalicText s1, ItalicText s2 -> s1 = s2
    | _, _ -> false

  (* Pretty-print a MarkdownText *)
  let pp fmt v =
    Format.fprintf fmt "%s" (to_string v)
end

(* Module for representing a complete Markdown line *)
module MarkdownLine = struct
  type t = MarkdownLine of MarkdownLineStarter.t option * MarkdownText.t list

  (* Convert a MarkdownLine to a string representation *)
  let to_string (MarkdownLine (starter, texts)) =
    let starter_str = match starter with
      | Some s -> MarkdownLineStarter.to_string s ^ " "
      | None -> ""
    in
    let texts_str = List.map MarkdownText.to_string texts |> String.concat ", "
    in
    "MarkdownLine(" ^ starter_str ^ "[" ^ texts_str ^ "])"

  (* Check equality of two MarkdownLines *)
  let equal (MarkdownLine (s1, t1)) (MarkdownLine (s2, t2)) =
    Option.equal MarkdownLineStarter.equal s1 s2
    && List.equal MarkdownText.equal t1 t2

  (* Pretty-print a MarkdownLine *)
  let pp fmt (MarkdownLine (starter, texts)) =
    Format.fprintf fmt "MarkdownLine(%a, %a)"
      (Format.pp_print_option MarkdownLineStarter.pp) starter
      (Format.pp_print_list MarkdownText.pp) texts
end

(* This function gets the markdownLineStarter of the line *)
let parseLineStart = function
  | '#' :: xn ->
    let rec countHeader acc = function
      | '#' :: rest -> countHeader (acc + 1) rest
      | rest -> acc, List.tl rest
    in let headerVal, restOfLine = countHeader 1 xn
    in Some (MarkdownLineStarter.Header headerVal), restOfLine
  | ' ' :: '*' :: ' ' :: xn -> Some (MarkdownLineStarter.UnorderedListItem), xn
  | xn -> None, xn

(* Module for representing different types of tokens in Markdown text *)
module TextToken = struct
  type t =
    | RegularTextToken of string
    | BoldToken
    | ItalicToken

  (* Convert a TextToken to a string representation *)
  let to_string = function
    | RegularTextToken s -> "RegularTextToken(" ^ s ^ ")"
    | BoldToken -> "BoldToken"
    | ItalicToken -> "ItalicToken"

  (* Check equality of two TextTokens *)
  let equal a b =
    match a, b with
    | RegularTextToken s1, RegularTextToken s2 -> s1 = s2
    | BoldToken, BoldToken -> true
    | ItalicToken, ItalicToken -> true
    | _, _ -> false

  (* Pretty-print a TextToken *)
  let pp fmt v =
    Format.fprintf fmt "%s" (to_string v)
end

(* This function tokenizes the text in the line *)
let tokenizeText text =
  (* Convert characters to tokens *)
  let rec tokenize = function
    | '*' :: '*' :: xn -> TextToken.ItalicToken :: tokenize xn
    | '*' :: xn -> TextToken.BoldToken :: tokenize xn
    | x :: xn -> TextToken.RegularTextToken (Char.escaped x) :: tokenize xn
    | [] -> []
  (* Combine adjacent RegularTextTokens *)
  in let rec combine = function
    | TextToken.RegularTextToken a :: TextToken.RegularTextToken b :: xn -> combine (TextToken.RegularTextToken (a ^ b) :: xn)
    | x :: xn -> x :: combine xn
    | [] -> []
  in text |> tokenize |> combine

(* This function parses tokens into MarkdownText *)
let rec parseTokens =
  let open TextToken
  in function
    | BoldToken :: RegularTextToken t :: BoldToken :: xn -> MarkdownText.BoldText t :: parseTokens xn
    | ItalicToken :: RegularTextToken t :: ItalicToken :: xn -> MarkdownText.ItalicText t :: parseTokens xn
    | RegularTextToken t :: xn -> MarkdownText.NormalText t :: parseTokens xn
    | [] -> []
    | _ -> failwith "Parsing Error"

(* This function parses a single line of Markdown *)
let parseLine line =
  let starter, rest = parseLineStart line
  in let tokenizedText = tokenizeText rest
  in let parsedText = parseTokens tokenizedText
  in MarkdownLine.MarkdownLine (starter, parsedText)

(* This function converts a sequence of characters to a sequence of lines *)
let rec convertSeqCharToSeqLine charSeq = match charSeq () with
  | Seq.Nil -> Seq.empty
  | _ -> match getFirstLine charSeq with
    | line, rest -> Seq.cons line (convertSeqCharToSeqLine rest)

(* TODO: unit test this function *)
(* This function parses the entire Markdown text *)
let parseText text = text
  |> convertSeqCharToSeqLine
  |> (Seq.map parseLine)

open Reader
open Reader.MarkdownText
open Reader.MarkdownLine

module HtmlToken = struct
  (* true means start, false means end *)
  type t =
    | Paragraph of bool
    | Text of string
    | Header of int * bool
    | Bold of bool
    | Italics of bool
    | ListItem of bool
    | UnorderedList of bool
    | NewLine

  (* Convert HtmlToken to its string representation *)
  let to_string = function
    | Text s -> s
    | Header (n, true) -> "<h" ^ string_of_int n ^ ">"
    | Header (n, false) -> "</h" ^ string_of_int n ^ ">"
    | Bold true -> "<strong>"
    | Bold false -> "</strong>"
    | Italics true -> "<em>"
    | Italics false -> "</em>"
    | ListItem true -> "<li>"
    | ListItem false -> "</li>"
    | UnorderedList true -> "<ul>"
    | UnorderedList false -> "</ul>"
    | NewLine -> "\n"
    | Paragraph true -> "<p>"
    | Paragraph false -> "</p>"

  (* Check equality of two HtmlTokens *)
  let equal a b = a = b

  (* Pretty print an HtmlToken *)
  let pp fmt t =
    Format.fprintf fmt "%s" (to_string t)
end

(* TODO: Write unit tests for this *)
(* Convert MarkdownText to a list of HtmlTokens *)
let convertTextToHtmlToken = let open HtmlToken in function
  | NormalText text -> [Text text]
  | BoldText text -> [Bold true; Text text; Bold false]
  | ItalicText text -> [Italics true; Text text; Italics false]

(* TODO: Write unit tests for this *)
(* Convert a MarkdownLine to a list of HtmlTokens *)
let convertMarkdownLine =
  let open HtmlToken in
  let convert = List.concat_map convertTextToHtmlToken
  in function
  | MarkdownLine (None, l) -> (Paragraph true) :: convert l @ [Paragraph false]
  | MarkdownLine (Some (MarkdownLineStarter.Header n), l) -> (Header (n, true)) :: convert l @ [Header (n, false)]
  | MarkdownLine (Some MarkdownLineStarter.UnorderedListItem , l) -> ListItem true :: convert l @ [ListItem false]

(* TODO: Write unit tests for this *)
(* TODO: Convert the optional argument into its own state type *)
(* TODO: Break this down into smaller functions if possible *)
(* Recursively convert a sequence of MarkdownLines to a sequence of HtmlToken lists *)
let rec convertLinesToTokens ?(unorderedList = false) seq = let open HtmlToken in
  match seq () with
  | Seq.Nil -> if unorderedList then Seq.cons [UnorderedList false] Seq.empty else Seq.empty
  | Seq.Cons (line, rest) ->
    let convertedLines = Seq.cons (convertMarkdownLine line) (Seq.cons [NewLine] (convertLinesToTokens rest))
    in let convertedLinesWithUOList = Seq.cons (convertMarkdownLine line) (Seq.cons [NewLine] (convertLinesToTokens ~unorderedList:true rest))
    in match line with
    | MarkdownLine (Some MarkdownLineStarter.UnorderedListItem, _) ->
      if not unorderedList then Seq.cons [UnorderedList true] convertedLinesWithUOList
      else convertedLinesWithUOList
    | _ ->
      if unorderedList then Seq.cons [UnorderedList false] convertedLines
      else convertedLines

(* Convert a sequence of MarkdownLines to a sequence of HTML strings *)
let convertLines lines =
    let convertSingleLine line = String.concat "" (List.map HtmlToken.to_string line)
    in Seq.map convertSingleLine (convertLinesToTokens lines)

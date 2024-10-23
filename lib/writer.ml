open Reader
open Reader.MarkdownText
open Reader.MarkdownLine

(* true means start, false means end *)
type htmlToken =
  | Paragraph of bool
  | Text of string
  | Header of int * bool
  | Bold of bool
  | Italics of bool
  | ListItem of bool
  | UnorderedList of bool
  | NewLine

let htmlTokenToString = function
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

(* TODO: Write unit tests for this *)
let convertTextToHtmlToken = function
  | NormalText text -> [Text text]
  | BoldText text -> [Bold true; Text text; Bold false]
  | ItalicText text -> [Italics true; Text text; Italics false]

(* TODO: Write unit tests for this *)
let convertMarkdownLine =
  let convert = List.concat_map convertTextToHtmlToken
  in function
  | MarkdownLine (None, l) -> (Paragraph true) :: convert l @ [Paragraph false]
  | MarkdownLine (Some (MarkdownLineStarter.Header n), l) -> (Header (n, true)) :: convert l @ [Header (n, false)]
  | MarkdownLine (Some MarkdownLineStarter.UnorderedListItem , l) -> ListItem true :: convert l @ [ListItem false]

(* TODO: Write unit tests for this *)
let rec convertLinesToTokens ?(unorderedList = false) seq = match seq () with
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

let convertLines lines =
    let convertSingleLine line = String.concat "" (List.map htmlTokenToString line)
    in Seq.map convertSingleLine (convertLinesToTokens lines)

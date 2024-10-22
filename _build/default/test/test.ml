open Alcotest
open Html2Markdown.Reader

(* Testing Getting First Line *)
module GetFirstLineTests = struct
  let input1= String.to_seq
  "This is line 1.\nThis is line 2.\nThis is line 3."

  let firstLine, input2 = getFirstLine input1
  let secondLine, input3 = getFirstLine input2
  let thirdLine, input4 = getFirstLine input3
  let emptyLine, _ = getFirstLine input4

  let test1 () = (check string) "first line" "This is line 1." (firstLine |> List.to_seq |> String.of_seq)
  let test2 () = (check string) "second line" "This is line 2." (secondLine |> List.to_seq |> String.of_seq)
  let test3 () = (check string) "third line" "This is line 3." (thirdLine |> List.to_seq |> String.of_seq)
  let test4 () = (check string) "empty line" "" (emptyLine |> List.to_seq |> String.of_seq)

  let tests = [
    "getFirstLine returns first line", `Quick, test1;
    "getFirstLine returns second line", `Quick, test2;
    "getFirstLine returns third line", `Quick, test3;
    "getFirstLine returns empty line", `Quick, test4;
  ]
end

(* Testing Getting the Line Starting Data *)
module ParseLineStartTests = struct
  open MarkdownLineStarter

  let input1 = "This is a regular sentence"
  let input2 = "# This is a header 1 sentence"
  let input3 = "## This is a header 2 sentence"
  let input4 = "-This is a bullet point"

  let start1, rest1 = parseLineStart (input1 |> String.to_seq |> List.of_seq)
  let start2, rest2 = parseLineStart (input2 |> String.to_seq |> List.of_seq)
  let start3, rest3 = parseLineStart (input3 |> String.to_seq |> List.of_seq)
  let start4, rest4 = parseLineStart (input4 |> String.to_seq |> List.of_seq)

  let testable_markdownLineStarter = testable (pp) (equal)

  let test1 () = check (Alcotest.option testable_markdownLineStarter) "Regular" None start1
  let test2 () = check string "Regular" "This is a regular sentence" (rest1 |> List.to_seq |> String.of_seq)
  let test3 () = check (Alcotest.option testable_markdownLineStarter) "Header 1" (Some (Header 1)) start2
  let test4 () = check string "Header 1" "This is a header 1 sentence" (rest2 |> List.to_seq |> String.of_seq)
  let test5 () = check (Alcotest.option testable_markdownLineStarter) "Header 2" (Some (Header 2)) start3
  let test6 () = check string "Header 2" "This is a header 2 sentence" (rest3 |> List.to_seq |> String.of_seq)
  let test7 () = check (Alcotest.option testable_markdownLineStarter) "Bullet Point" (Some UnorderedListItem) start4
  let test8 () = check string "Bullet Point" "This is a bullet point" (rest4 |> List.to_seq |> String.of_seq)

  let tests = [
    "Regular sentence start", `Quick, test1;
    "Regular sentence rest", `Quick, test2;
    "Header 1 start", `Quick, test3;
    "Header 1 rest", `Quick, test4;
    "Header 2 start", `Quick, test5;
    "Header 2 rest", `Quick, test6;
    "Bullet point start", `Quick, test7;
    "Bullet point rest", `Quick, test8;
  ]
end

module TokenizeTextTests = struct
  open TextToken

  let input1 = "This is a regular sentence of text"
  let input2 = "This is text with *bold* stuff"
  let input3 = "This is text with **italic** stuff"

  let expOutput1 = [RegularTextToken "This is a regular sentence of text"]
  let expOutput2 = [RegularTextToken "This is text with "; BoldToken; RegularTextToken "bold"; BoldToken; RegularTextToken " stuff"]
  let expOutput3 = [RegularTextToken "This is text with "; ItalicToken; RegularTextToken "italic"; ItalicToken; RegularTextToken " stuff"]


  let testable_textToken = testable (pp) (equal)

  let test1 () = check (Alcotest.list testable_textToken) "Regular" expOutput1 (input1 |> String.to_seq |> List.of_seq |> tokenizeText)
  let test2 () = check (Alcotest.list testable_textToken) "Bold" expOutput2 (input2 |> String.to_seq |> List.of_seq |> tokenizeText)
  let test3 () = check (Alcotest.list testable_textToken) "Italic" expOutput3 (input3 |> String.to_seq |> List.of_seq |> tokenizeText)

  let tests = [
    "Tokenize regular text", `Quick, test1;
    "Tokenize bold text", `Quick, test2;
    "Tokenize italic text", `Quick, test3;
  ]
end

let () =
  run "Html2Markdown Tests" [
    "Getting the first line", GetFirstLineTests.tests;
    "Checking if it can parse how the line starts", ParseLineStartTests.tests;
    "Checking if text can be tokenized correctly", TokenizeTextTests.tests;
  ]

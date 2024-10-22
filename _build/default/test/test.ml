open Alcotest
open Html2Markdown

(* Testing Getting First Line *)
module GetFirstLineTests = struct
  let tokenizerInput1 = String.to_seq
  "This is line 1.\nThis is line 2.\nThis is line 3."

  let firstLine, tokenizerInput2 = Reader.getFirstLine tokenizerInput1
  let secondLine, tokenizerInput3 = Reader.getFirstLine tokenizerInput2
  let thirdLine, tokenizerInput4 = Reader.getFirstLine tokenizerInput3
  let emptyLine, _ = Reader.getFirstLine tokenizerInput4

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
  let parsingStartInput1 = "This is a regular sentence"
  let parsingStartInput2 = "# This is a header 1 sentence"
  let parsingStartInput3 = "## This is a header 2 sentence"
  let parsingStartInput4 = "-This is a bullet point"

  let start1, rest1 = Reader.parseLineStart (parsingStartInput1 |> String.to_seq |> List.of_seq)
  let start2, rest2 = Reader.parseLineStart (parsingStartInput2 |> String.to_seq |> List.of_seq)
  let start3, rest3 = Reader.parseLineStart (parsingStartInput3 |> String.to_seq |> List.of_seq)
  let start4, rest4 = Reader.parseLineStart (parsingStartInput4 |> String.to_seq |> List.of_seq)

  let testable_markdownLineStarter = testable (Reader.MarkdownLineStarter.pp) (Reader.MarkdownLineStarter.equal_markdownLineStarter)

  let test1 () = check (Alcotest.option testable_markdownLineStarter) "Regular" None start1
  let test2 () = check string "Regular" "This is a regular sentence" (rest1 |> List.to_seq |> String.of_seq)
  let test3 () = check (Alcotest.option testable_markdownLineStarter) "Header 1" (Some (Reader.Header 1)) start2
  let test4 () = check string "Header 1" "This is a header 1 sentence" (rest2 |> List.to_seq |> String.of_seq)
  let test5 () = check (Alcotest.option testable_markdownLineStarter) "Header 2" (Some (Reader.Header 2)) start3
  let test6 () = check string "Header 2" "This is a header 2 sentence" (rest3 |> List.to_seq |> String.of_seq)
  let test7 () = check (Alcotest.option testable_markdownLineStarter) "Bullet Point" (Some Reader.UnorderedListItem) start4
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

let () =
  run "Html2Markdown Tests" [
    "Getting the first line", GetFirstLineTests.tests;
    "Checking if it can parse how the line starts", ParseLineStartTests.tests;
  ]

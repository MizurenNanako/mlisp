let () =
  let lexbuf =
    Sys.argv.(1) |> In_channel.open_text |> Lexing.from_channel
  in
  Lexing.set_filename lexbuf Sys.argv.(1);
  match Parse.Driver.run lexbuf with
  | Some ast ->
      ast
      |> List.map Syntactics.AST.to_string
      |> List.iter print_endline
  | None -> print_endline "got error"

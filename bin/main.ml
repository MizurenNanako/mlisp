let () =
  let lexbuf =
    Sys.argv.(1) |> In_channel.open_text |> Lexing.from_channel
  in
  Lexing.set_filename lexbuf Sys.argv.(1);
  Parse.Driver.run lexbuf
  |> List.map Syntactics.AST.to_string_with_rng
  |> List.iter print_endline

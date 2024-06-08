let () =
  let lexbuf =
    Sys.argv.(1) |> In_channel.open_text |> Lexing.from_channel
  in
  Lexing.set_filename lexbuf Sys.argv.(1);
  let raw_ast = Parse.Driver.run lexbuf in
  raw_ast
  |> List.map Syntactics.AST.to_string_with_rng
  |> List.iter print_endline;
  let astlist = raw_ast |> List.map Semantics.Eval.transform in
  print_endline "raw:";
  astlist
  |> List.map Typing.Anno.sexp_of_t
  |> List.iter (Sexplib.Sexp.output_mach stdout)
  |> print_newline;
  print_endline "evaluated:";
  astlist |> Typing.Anno.evalseq []
  |> List.map Typing.Anno.sexp_of_t
  |> List.iter (Printf.printf "%a\n" Sexplib.Sexp.output_mach)

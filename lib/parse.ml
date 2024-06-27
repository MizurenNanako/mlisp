module Driver = struct
  module I = Parser.MenhirInterpreter
  module A = Syntactics.AST
  module Msg = Error_messages

  let rec _run
    (lex : unit -> Lexical.Token.token)
    (checkpoint : A.expr list I.checkpoint)
    (lastpoint : A.expr list I.checkpoint)
    =
    match checkpoint with
    | I.Accepted v -> v
    | I.Rejected -> []
    | I.AboutToReduce _ ->
      let cp = I.resume checkpoint in
      _run lex cp lastpoint
    | I.Shifting _ ->
      let cp = I.resume checkpoint in
      _run lex cp lastpoint
    | I.InputNeeded _ ->
      let tk =
        try lex () with
        | Lexical.Error.LexicalError (msg, _) ->
          Printf.eprintf
            "LexicalError: %s at %a\n"
            msg
            Lexical.Range.dump
            (Lexer.lb () |> Lexical.Range.of_lexbuf);
          exit 0
      in
      let pL = Lexer.start_p () in
      let pR = Lexer.curr_p () in
      let cp = I.offer checkpoint (tk, pL, pR) in
      _run lex cp checkpoint
    | I.HandlingError e ->
      let state_num = I.current_state_number e in
      let msg = Msg.message state_num in
      Printf.eprintf
        "SyntaxError: at %a\n%s\n"
        Lexical.Range.dump
        (Lexer.lb () |> Lexical.Range.of_lexbuf)
        msg;
      exit (-1)
  ;;

  let run (filename : string) : A.expr list =
    let lex = Lexer.init filename in
    let startpoint = Parser.Incremental.start (Lexer.curr_p ()) in
    _run lex startpoint startpoint
  ;;
end

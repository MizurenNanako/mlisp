module Error = struct
  let range_to_string ((a : Lexing.position), (b : Lexing.position)) =
    if a.pos_fname = b.pos_fname then
      if a.pos_lnum = b.pos_lnum then
        Printf.sprintf "%s:%i:%i-%i" a.pos_fname a.pos_lnum
          (a.pos_cnum - a.pos_bol + 1)
          (b.pos_cnum - b.pos_bol + 1)
      else
        Printf.sprintf "%s:%i:%i-%i:%i" a.pos_fname a.pos_lnum
          (a.pos_cnum - a.pos_bol + 1)
          b.pos_lnum
          (b.pos_cnum - b.pos_bol + 1)
    else
      Printf.sprintf "%s:%i:%i-%s:%i:%i" a.pos_fname a.pos_lnum
        (a.pos_cnum - a.pos_bol + 1)
        b.pos_fname b.pos_lnum
        (b.pos_cnum - b.pos_bol + 1)

  let report_range out rng =
    rng |> range_to_string |> output_string out
end

module Driver = struct
  module L = Lexer
  module P = Parser
  module T = Lexical.Token
  module I = P.MenhirInterpreter
  module A = Syntactics.AST

  let run_with_supplier (supplier : I.supplier) startpoint =
    let rec loop savepoint (checkpoint : A.t list I.checkpoint) =
      match checkpoint with
      | Accepted v -> Some v
      | Rejected -> None
      | InputNeeded _ ->
          let tk, pos1, pos2 = supplier () in
          let nextpoint = I.offer checkpoint (tk, pos1, pos2) in
          loop checkpoint nextpoint
      | Shifting _ | AboutToReduce _ ->
          let nextpoint = I.resume checkpoint in
          loop savepoint nextpoint
      | HandlingError env -> (
          let r1, r2 = I.positions env in
          match I.acceptable savepoint T.RP r2 with
          | true ->
              let fixed = I.offer savepoint (T.RP, r1, r2) in
              Printf.eprintf "Inserted \')\' in %a\n" Error.report_range
                (r1, r2);
              loop savepoint fixed
          | false ->
              Printf.eprintf "FatalError in %a\n" Error.report_range
                (r1, r2);
              None)
    in
    loop startpoint startpoint

  let run lexbuf =
    let supplier = I.lexer_lexbuf_to_supplier L.get_token lexbuf in
    let startpoint = P.Incremental.program lexbuf.lex_curr_p in
    run_with_supplier supplier startpoint
end

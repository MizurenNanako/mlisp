module Error = struct
  exception LexicalError of string
end

module Range = struct
  type pos = Lexing.position
  type range = pos * pos
  type t = range

  let join (a : t) (b : t) = (fst a, snd b)
  let of_lexbuf (a : Lexing.lexbuf) : t = (a.lex_start_p, a.lex_curr_p)

  let to_string ((a : Lexing.position), (b : Lexing.position)) =
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

  let str ((s : Lexing.position), (e : Lexing.position)) =
    if s.pos_lnum = e.pos_lnum then
      Printf.sprintf "%i:%i-%i" s.pos_lnum
        (s.pos_cnum - s.pos_bol + 1)
        (e.pos_cnum - e.pos_bol + 1)
    else
      Printf.sprintf "%i:%i-%i:%i" s.pos_lnum
        (s.pos_cnum - s.pos_bol + 1)
        e.pos_lnum
        (e.pos_cnum - e.pos_bol + 1)
end

module Ranged = struct
  type range = Range.t
  type 'a t = 'a * range

  let rngstr (x : 'a t) : string =
    let s, e = snd x in
    Printf.sprintf "%i:%i-%i:%i" s.pos_lnum
      (s.pos_cnum - s.pos_bol + 1)
      e.pos_lnum
      (e.pos_cnum - e.pos_bol + 1)
end

module Token = struct
  type 'a ranged = 'a Ranged.t

  type token =
    | LP of Range.t
    | RP of Range.t
    | Tstring of string ranged
    | QUOTE of Range.t
    | CXR of bool list ranged
    | EOF

  let to_string (t : token) =
    match t with
    | LP _ -> "("
    | RP _ -> ")"
    | Tstring (str, r) -> str ^ Range.str r
    | QUOTE _ -> "\'"
    | CXR (l, r) ->
        (l
        |> List.map (fun c ->
               match c with false -> 'a' | true -> 'd')
        |> List.to_seq |> String.of_seq)
        ^ Range.str r
    | EOF -> "#"

  let dump out t = t |> to_string |> output_string out
end

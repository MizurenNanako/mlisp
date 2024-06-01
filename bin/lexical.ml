module Error = struct
  exception LexicalError of string
end

module Token = struct
  type token = LP | RP | Tstring of string | QUOTE | EOF

  let to_string (t : token) =
    match t with
    | LP -> "("
    | RP -> ")"
    | Tstring str -> str
    | QUOTE -> "\'"
    | EOF -> "#"

  let dump out t = t |> to_string |> output_string out
end

module Range = struct
  type pos = Lexing.position
  type range = pos * pos
  type t = range
  type 'a ranged = [ `Ranged of 'a * range ]

  let join (a : t) (b : t) = (fst a, snd b)
  let extract_range (`Ranged (_, b) : 'a ranged) = b

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
end

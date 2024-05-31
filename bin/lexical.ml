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

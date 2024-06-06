module AST = struct
  type range = Lexical.Range.t

  module R = Lexical.Range

  type expression =
    | Atom of string * range (* a sequence of letters *)
    | List of expression list * range
  (* zero or more expressions separated
     by whitespace and enclosed by parentheses *)

  type t = expression

  let get_rng = function Atom (_, r) | List (_, r) -> r

  let rec to_string_with_rng a =
    let open Printf in
    match a with
    | Atom (s, r) -> sprintf "\027[33m%s \027[32m%s\027[0m" s (R.str r)
    | List (l, r) ->
        let s1 = l |> List.map to_string_with_rng |> String.concat " " in
        let s2 = R.str r in
        sprintf "\027[31m(%s\027[31m)\027[34m%s\027[0m" s1 s2

  let rec to_string a =
    let open Printf in
    match a with
    | Atom (s, _) -> sprintf "\"%s\"" s
    | List (l, _) ->
        let s1 = l |> List.map to_string |> String.concat " " in
        sprintf "(%s)" s1

  let dump_list = Common.dump_list

  let rec dump_rng out a =
    let open Printf in
    match a with
    | Atom (s, r) -> fprintf out "\"%s\"<%s>" s (R.str r)
    | List (l, r) ->
        fprintf out "%a<%s>" (dump_list dump_rng) l (R.str r)

  let rec dump out a =
    let open Printf in
    match a with
    | Atom (s, _) -> fprintf out "\"%s\"" s
    | List (l, _) -> fprintf out "%a" (dump_list dump) l
end

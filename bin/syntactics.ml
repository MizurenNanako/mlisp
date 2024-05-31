module AST = struct
  type expression =
    | Atom of string (* a sequence of letters *)
    | List of expression list
  (* zero or more expressions separated
     by whitespace and enclosed by parentheses *)

  type t = expression

  let rec to_string a =
    let open Printf in
    match a with
    | Atom s -> sprintf "\"%s\"" s
    | List l ->
        l |> List.map to_string |> String.concat " " |> sprintf "(%s)"

  let rec dump out a =
    let open Printf in
    match a with
    | Atom s -> fprintf out "\"%s\"" s
    | List l -> dump_list out l

  and dump_list out l =
    let open Printf in
    match l with
    | [] -> fprintf out ")"
    | [ a ] -> fprintf out "%a" dump a
    | a :: tl ->
        fprintf out "%a " dump a;
        dump_list out tl
end

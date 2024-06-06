let rec dump_list dump sep out l =
  let open Printf in
  match l with
  | [] -> fprintf out ")"
  | [ a ] -> fprintf out "%a" dump a
  | a :: tl ->
      fprintf out "%a%s" dump a sep;
      (dump_list dump sep) out tl

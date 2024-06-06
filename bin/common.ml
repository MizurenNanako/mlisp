let rec dump_list dump out l =
  let open Printf in
  match l with
  | [] -> fprintf out ")"
  | [ a ] -> fprintf out "%a" dump a
  | a :: tl ->
      fprintf out "%a " dump a;
      (dump_list dump) out tl

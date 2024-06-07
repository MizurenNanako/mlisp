let rec dump_list dump sep out l =
  let open Printf in
  match l with
  | [] -> fprintf out ")"
  | [ a ] -> fprintf out "%a" dump a
  | a :: tl ->
      fprintf out "%a%s" dump a sep;
      (dump_list dump sep) out tl

(** [take_first_n n lst] takes the first [n] element of list [lst] 
    and returns the list token. Assuming [n >= List.length lst]*)
let take_first_n (n : int) (lst : 'a list) : 'a list =
  let rec take acc n lst =
    match lst with
    | [] -> acc
    | hd :: tl -> take (hd :: acc) (n - 1) tl
  in
  take [] n lst

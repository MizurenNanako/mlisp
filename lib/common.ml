let rec tail l =
  match l with
  | [] -> []
  | [ a ] -> a
  | _ :: tl -> tail tl
;;

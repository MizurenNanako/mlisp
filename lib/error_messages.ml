
(* This file was auto-generated based on "error_message.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 93 ->
        "Ends in an error in state: 93.\nbind_expr -> id Tbind expr . Tin expr [ Tstr Tlp Tlc Tid Ti64 Tf64 Teof ]\ntop_bind_expr -> id Tbind expr . [ Tstr Tlp Tlc Tid Ti64 Tf64 Teof ]\nThe known suffix of the stack is as follows:\nid Tbind expr\n"
    | 92 ->
        "Ends in an error in state: 92.\nbind_expr -> id Tbind . expr Tin expr [ Tstr Tlp Tlc Tid Ti64 Tf64 Teof ]\ntop_bind_expr -> id Tbind . expr [ Tstr Tlp Tlc Tid Ti64 Tf64 Teof ]\nThe known suffix of the stack is as follows:\nid Tbind\n"
    | 90 ->
        "Ends in an error in state: 90.\ndecl_expr -> id Tcolon str . type_expr [ Tstr Tlp Tlc Tid Ti64 Tf64 Teof ]\nThe known suffix of the stack is as follows:\nid Tcolon str\n"
    | 89 ->
        "Ends in an error in state: 89.\ndecl_expr -> id Tcolon type_expr . [ Tstr Tlp Tlc Tid Ti64 Tf64 Teof ]\nlocal_decl_expr -> id Tcolon type_expr . Tin local_decl_expr [ Tstr Tlp Tlc Tid Ti64 Tf64 Teof ]\nlocal_decl_expr -> id Tcolon type_expr . Teq expr Tin expr [ Tstr Tlp Tlc Tid Ti64 Tf64 Teof ]\nThe known suffix of the stack is as follows:\nid Tcolon type_expr\n"
    | 87 ->
        "Ends in an error in state: 87.\ndecl_expr -> id Tcolon . type_expr [ Tstr Tlp Tlc Tid Ti64 Tf64 Teof ]\ndecl_expr -> id Tcolon . str type_expr [ Tstr Tlp Tlc Tid Ti64 Tf64 Teof ]\nlocal_decl_expr -> id Tcolon . type_expr Tin local_decl_expr [ Tstr Tlp Tlc Tid Ti64 Tf64 Teof ]\nlocal_decl_expr -> id Tcolon . type_expr Teq expr Tin expr [ Tstr Tlp Tlc Tid Ti64 Tf64 Teof ]\nThe known suffix of the stack is as follows:\nid Tcolon\n"
    | 83 ->
        "Ends in an error in state: 83.\nexport_expr -> str Tbind . id [ Tstr Tlp Tlc Tid Ti64 Tf64 Teof ]\nThe known suffix of the stack is as follows:\nstr Tbind\n"
    | 80 ->
        "Ends in an error in state: 80.\nlist(top_expr) -> top_expr . list(top_expr) [ Teof ]\nThe known suffix of the stack is as follows:\ntop_expr\n"
    | 79 ->
        "Ends in an error in state: 79.\nlocal_decl_expr -> id Tcolon type_expr . Tin local_decl_expr [ Trp ]\nlocal_decl_expr -> id Tcolon type_expr . Teq expr Tin expr [ Trp ]\nparam_ -> id Tcolon type_expr . [ Trp Tcomma ]\nThe known suffix of the stack is as follows:\nid Tcolon type_expr\n"
    | 78 ->
        "Ends in an error in state: 78.\nlocal_decl_expr -> id Tcolon . type_expr Tin local_decl_expr [ Trp ]\nlocal_decl_expr -> id Tcolon . type_expr Teq expr Tin expr [ Trp ]\nparam_ -> id Tcolon . type_expr [ Trp Tcomma ]\nThe known suffix of the stack is as follows:\nid Tcolon\n"
    | 75 ->
        "Ends in an error in state: 75.\nlambda_expr -> Tlp separated_nonempty_list(Tcomma,param_)? Trp Tto . expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nTlp separated_nonempty_list(Tcomma,param_)? Trp Tto\n"
    | 74 ->
        "Ends in an error in state: 74.\nlambda_expr -> Tlp separated_nonempty_list(Tcomma,param_)? Trp . Tto expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nTlp separated_nonempty_list(Tcomma,param_)? Trp\n"
    | 71 ->
        "Ends in an error in state: 71.\nparam_ -> id Tcolon . type_expr [ Trp Tcomma ]\nThe known suffix of the stack is as follows:\nid Tcolon\n"
    | 70 ->
        "Ends in an error in state: 70.\nparam_ -> id . Tcolon type_expr [ Trp Tcomma ]\nThe known suffix of the stack is as follows:\nid\n"
    | 68 ->
        "Ends in an error in state: 68.\nseparated_nonempty_list(Tcomma,param_) -> param_ Tcomma . separated_nonempty_list(Tcomma,param_) [ Trp ]\nThe known suffix of the stack is as follows:\nparam_ Tcomma\n"
    | 67 ->
        "Ends in an error in state: 67.\nseparated_nonempty_list(Tcomma,param_) -> param_ . [ Trp ]\nseparated_nonempty_list(Tcomma,param_) -> param_ . Tcomma separated_nonempty_list(Tcomma,param_) [ Trp ]\nThe known suffix of the stack is as follows:\nparam_\n"
    | 64 ->
        "Ends in an error in state: 64.\nseparated_nonempty_list(Tcomma,expr) -> expr Tcomma . separated_nonempty_list(Tcomma,expr) [ Trc ]\nThe known suffix of the stack is as follows:\nexpr Tcomma\n"
    | 63 ->
        "Ends in an error in state: 63.\nseparated_nonempty_list(Tcomma,expr) -> expr . [ Trc ]\nseparated_nonempty_list(Tcomma,expr) -> expr . Tcomma separated_nonempty_list(Tcomma,expr) [ Trc ]\nThe known suffix of the stack is as follows:\nexpr\n"
    | 61 ->
        "Ends in an error in state: 61.\nbind_expr -> id Tbind expr Tin . expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nid Tbind expr Tin\n"
    | 60 ->
        "Ends in an error in state: 60.\nbind_expr -> id Tbind expr . Tin expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nid Tbind expr\n"
    | 59 ->
        "Ends in an error in state: 59.\nbind_expr -> id Tbind . expr Tin expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nid Tbind\n"
    | 58 ->
        "Ends in an error in state: 58.\ncall_expr -> call_expr . Tlb separated_nonempty_list(Tcomma,call_expr)? Trb [ Tto Tlb ]\ncond_ -> call_expr . Tto call_expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma Tbar ]\nThe known suffix of the stack is as follows:\ncall_expr\n"
    | 55 ->
        "Ends in an error in state: 55.\nseparated_nonempty_list(Tcomma,call_expr) -> call_expr Tcomma . separated_nonempty_list(Tcomma,call_expr) [ Trb ]\nThe known suffix of the stack is as follows:\ncall_expr Tcomma\n"
    | 54 ->
        "Ends in an error in state: 54.\ncall_expr -> call_expr . Tlb separated_nonempty_list(Tcomma,call_expr)? Trb [ Trb Tlb Tcomma ]\nseparated_nonempty_list(Tcomma,call_expr) -> call_expr . [ Trb ]\nseparated_nonempty_list(Tcomma,call_expr) -> call_expr . Tcomma separated_nonempty_list(Tcomma,call_expr) [ Trb ]\nThe known suffix of the stack is as follows:\ncall_expr\n"
    | 50 ->
        "Ends in an error in state: 50.\ncall_expr -> call_expr Tlb . separated_nonempty_list(Tcomma,call_expr)? Trb [ Tto Tstr Trp Trc Trb Tlp Tlc Tlb Tin Tid Ti64 Tf64 Teof Tcomma Tbar ]\nThe known suffix of the stack is as follows:\ncall_expr Tlb\n"
    | 49 ->
        "Ends in an error in state: 49.\ncall_expr -> call_expr . Tlb separated_nonempty_list(Tcomma,call_expr)? Trb [ Tstr Trp Trc Tlp Tlc Tlb Tin Tid Ti64 Tf64 Teof Tcomma Tbar ]\ncond_ -> call_expr Tto call_expr . [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma Tbar ]\nThe known suffix of the stack is as follows:\ncall_expr Tto call_expr\n"
    | 47 ->
        "Ends in an error in state: 47.\ncond_ -> call_expr Tto . call_expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma Tbar ]\nThe known suffix of the stack is as follows:\ncall_expr Tto\n"
    | 46 ->
        "Ends in an error in state: 46.\ncall_expr -> call_expr . Tlb separated_nonempty_list(Tcomma,call_expr)? Trb [ Tto Tstr Trp Trc Tlp Tlc Tlb Tin Tid Ti64 Tf64 Teof Tcomma ]\ncond_ -> call_expr . Tto call_expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma Tbar ]\ncond_expr -> call_expr . [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\ncall_expr\n"
    | 41 ->
        "Ends in an error in state: 41.\nprimary_expr -> Tlp expr . Trp [ Tto Tstr Trp Trc Trb Tlp Tlc Tlb Tin Tid Ti64 Tf64 Teof Tcomma Tbar ]\nThe known suffix of the stack is as follows:\nTlp expr\n"
    | 40 ->
        "Ends in an error in state: 40.\nprimary_expr -> Tlp . expr Trp [ Tto Tstr Trp Trc Trb Tlp Tlc Tlb Tin Tid Ti64 Tf64 Teof Tcomma Tbar ]\nThe known suffix of the stack is as follows:\nTlp\n"
    | 39 ->
        "Ends in an error in state: 39.\ncond_rev -> cond_rev Tbar . cond_ [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma Tbar ]\nThe known suffix of the stack is as follows:\ncond_rev Tbar\n"
    | 36 ->
        "Ends in an error in state: 36.\nlocal_decl_expr -> id Tcolon type_expr Teq expr Tin . expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nid Tcolon type_expr Teq expr Tin\n"
    | 35 ->
        "Ends in an error in state: 35.\nlocal_decl_expr -> id Tcolon type_expr Teq expr . Tin expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nid Tcolon type_expr Teq expr\n"
    | 34 ->
        "Ends in an error in state: 34.\nlocal_decl_expr -> id Tcolon type_expr Teq . expr Tin expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nid Tcolon type_expr Teq\n"
    | 31 ->
        "Ends in an error in state: 31.\nlocal_decl_expr -> id Tcolon type_expr Tin . local_decl_expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nid Tcolon type_expr Tin\n"
    | 30 ->
        "Ends in an error in state: 30.\nlocal_decl_expr -> id Tcolon type_expr . Tin local_decl_expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nlocal_decl_expr -> id Tcolon type_expr . Teq expr Tin expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nid Tcolon type_expr\n"
    | 28 ->
        "Ends in an error in state: 28.\ntype_expr -> Tlp type_expr . Trp [ Tstr Trp Trb Tlp Tlc Tin Tid Ti64 Tf64 Teq Teof Tcomma ]\nThe known suffix of the stack is as follows:\nTlp type_expr\n"
    | 26 ->
        "Ends in an error in state: 26.\ntype_expr -> Tlb separated_nonempty_list(Tcomma,type_expr)? Trb Tto . type_expr [ Tstr Trp Trb Tlp Tlc Tin Tid Ti64 Tf64 Teq Teof Tcomma ]\nThe known suffix of the stack is as follows:\nTlb separated_nonempty_list(Tcomma,type_expr)? Trb Tto\n"
    | 25 ->
        "Ends in an error in state: 25.\ntype_expr -> Tlb separated_nonempty_list(Tcomma,type_expr)? Trb . Tto type_expr [ Tstr Trp Trb Tlp Tlc Tin Tid Ti64 Tf64 Teq Teof Tcomma ]\nThe known suffix of the stack is as follows:\nTlb separated_nonempty_list(Tcomma,type_expr)? Trb\n"
    | 20 ->
        "Ends in an error in state: 20.\nseparated_nonempty_list(Tcomma,type_expr) -> type_expr Tcomma . separated_nonempty_list(Tcomma,type_expr) [ Trb ]\nThe known suffix of the stack is as follows:\ntype_expr Tcomma\n"
    | 19 ->
        "Ends in an error in state: 19.\nseparated_nonempty_list(Tcomma,type_expr) -> type_expr . [ Trb ]\nseparated_nonempty_list(Tcomma,type_expr) -> type_expr . Tcomma separated_nonempty_list(Tcomma,type_expr) [ Trb ]\nThe known suffix of the stack is as follows:\ntype_expr\n"
    | 17 ->
        "Ends in an error in state: 17.\ntype_expr -> Tlb . separated_nonempty_list(Tcomma,type_expr)? Trb Tto type_expr [ Tstr Trp Trb Tlp Tlc Tin Tid Ti64 Tf64 Teq Teof Tcomma ]\nThe known suffix of the stack is as follows:\nTlb\n"
    | 16 ->
        "Ends in an error in state: 16.\ntype_expr -> Tlp . type_expr Trp [ Tstr Trp Trb Tlp Tlc Tin Tid Ti64 Tf64 Teq Teof Tcomma ]\nThe known suffix of the stack is as follows:\nTlp\n"
    | 15 ->
        "Ends in an error in state: 15.\nlocal_decl_expr -> id Tcolon . type_expr Tin local_decl_expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nlocal_decl_expr -> id Tcolon . type_expr Teq expr Tin expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nid Tcolon\n"
    | 14 ->
        "Ends in an error in state: 14.\nbind_expr -> id . Tbind expr Tin expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nlocal_decl_expr -> id . Tcolon type_expr Tin local_decl_expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nlocal_decl_expr -> id . Tcolon type_expr Teq expr Tin expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nid\n"
    | 5 ->
        "Ends in an error in state: 5.\nid -> Tid . [ Tcolon Tbind ]\nprimary_expr -> Tid . [ Tto Tstr Trp Trc Tlp Tlc Tlb Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nTid\n"
    | 4 ->
        "Ends in an error in state: 4.\nprimary_expr -> Tlc . separated_nonempty_list(Tcomma,expr)? Trc [ Tto Tstr Trp Trc Trb Tlp Tlc Tlb Tin Tid Ti64 Tf64 Teof Tcomma Tbar ]\nThe known suffix of the stack is as follows:\nTlc\n"
    | 2 ->
        "Ends in an error in state: 2.\nlambda_expr -> Tlp . separated_nonempty_list(Tcomma,param_)? Trp Tto expr [ Tstr Trp Trc Tlp Tlc Tin Tid Ti64 Tf64 Teof Tcomma ]\nprimary_expr -> Tlp . expr Trp [ Tto Tstr Trp Trc Tlp Tlc Tlb Tin Tid Ti64 Tf64 Teof Tcomma ]\nThe known suffix of the stack is as follows:\nTlp\n"
    | 1 ->
        "Ends in an error in state: 1.\nprimary_expr -> Tstr . [ Tto Tstr Tlp Tlc Tlb Tid Ti64 Tf64 Teof ]\nstr -> Tstr . [ Tbind ]\nThe known suffix of the stack is as follows:\nTstr\n"
    | 0 ->
        "Ends in an error in state: 0.\nstart' -> . start [ # ]\nThe known suffix of the stack is as follows:\n<empty>\n"
    | _ ->
        raise Not_found

type toktype =
    | Whitespace
    | End
    | Lparen
    | Rparen
    | Op
    | Num
    | Str
    | Comma
    | Lbrack
    | Rbrack
    | Lbrace
    | Rbrace
;;

let ttype_to_str t =
    match t with
    | Whitespace -> "w"
    | End -> "end"
    | Lparen -> "("
    | Rparen -> ")"
    | Op -> "op"
    | Num -> "num"
    | Str -> "str"
    | Comma -> "comma"
    | Lbrack -> "["
    | Rbrack -> "]"
    | Lbrace -> "{"
    | Rbrace -> "}"
;;

let ttype_lst_to_str lst = List.fold_left (fun acc t -> acc ^ (ttype_to_str t) ^ ", ") "" lst;;

module CharMap = Map.Make(Char);;

let toktypes : (toktype list) CharMap.t = CharMap.of_seq @@ List.to_seq [
    (' ',  [Whitespace]);
    ('\n', [Whitespace]);
    ('\t', [Whitespace]);
    (';',  [End]);
    ('(',  [Lparen]);
    (')',  [Rparen]);
    ('[',  [Lbrack]);
    (']',  [Rbrack]);
    ('{',  [Lbrace]);
    ('}',  [Rbrace]);
    ('.',  [Op; Num]);
    (',',  [Comma]);
];;

let print_ttype_map ttmap = CharMap.iter (fun x y -> print_endline ("{" ^ (String.make 1 x) ^ "}: " ^ (ttype_lst_to_str y))) ttmap;;

module ToktypeSet = Set.Make(struct
    type t = toktype
    let compare a b = if a = b then 0 else if a < b then -1 else 1
end);;

let ttype_set_from_list = List.fold_left (fun acc e -> ToktypeSet.add e acc) ToktypeSet.empty;;
let independent_toktypes = ttype_set_from_list [
    Lparen;
    Rparen;
    Lbrack;
    Rbrack;
    Lbrace;
    Rbrace;
    End;
    Comma;
];;

let set_toktype lst ttype toktypes = List.fold_left (fun toktypes x -> CharMap.add x ttype toktypes) toktypes lst;;
let str_to_list s =
    let rec helper s n =
        if n < 0 then []
        else s.[n] :: (helper s (n-1))
    in helper s (String.length s - 1)
;;

let toktypes = set_toktype ['~'; '!'; '@'; '#'; '$'; '%'; '^'; '&'; '*'; '-'; '+'; '='; '/'; '|'; '<'; '>';] [Op;] toktypes;;
let toktypes = set_toktype ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';] [Num; Str] toktypes;;
let toktypes = set_toktype (str_to_list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_") [Str;] toktypes;;

let rec get_next_token s i ttype =
    if i >= String.length s then ("", i)
    else
        let cttype = CharMap.find s.[i] toktypes in
        if cttype = [Whitespace;] then
            if ttype = Whitespace then get_next_token s (i + 1) ttype
            else ("", i)
        else if ToktypeSet.mem (List.hd cttype) independent_toktypes then
            if ttype = Whitespace then (String.make 1 (s.[i]), i + 1)
            else ("", i)
        else if List.mem ttype cttype || ttype = Whitespace then
            let res = get_next_token s (i + 1) (List.hd cttype) in
            let str = fst res in
            let index = snd res in
            ((String.make 1 s.[i]) ^ str, index)
        else
            ("", i)
;;

let get_all_tokens s =
    let rec helper s i =
        if i >= String.length s then []
        else
            let res = get_next_token s i Whitespace in
            let tok = fst res in
            let index = snd res in
            if tok = "" then helper s index
            else tok :: (helper s index)
    in helper s 0
;;

(*let test = get_all_tokens "[1,2,3];";;

let print_token_list lst = List.iter print_endline lst;;
print_token_list test;; *)

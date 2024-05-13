type printable_type = string;;

type abstract_type =
    | None
    | Gobble
    | Match of abstract_type
    | End
    | Op of abstract_type
    | Num
    | Lparen
    | Rparen of abstract_type
    | Lbrack of abstract_type
    | Rbrack
    | List of abstract_type
    | Period
    | Index
    | Let
    | Letvar
    | Leteq
    | Equal
    | Primitive
    | Comma of (abstract_type list)
    | ListRparen of (abstract_type list)
    | Product of (abstract_type list)
    | Lbrace
    | Rbrace
    | AltLparen
    | Plist
    | AltLbrace
    | Code
    | Fun
    | Funname
    | Funvars
    | Closure
;;

let rec atype_to_str t =
    match t with
    | None -> "None"
    | Gobble -> "Gobble"
    | Match(t) -> "Match{" ^ (atype_to_str t) ^ "}"
    | End -> "End"
    | Op(t) -> "Op{" ^ (atype_to_str t) ^ "}"
    | Num -> "Num"
    | Lparen -> "Lparen"
    | Rparen(t) -> "Rparen{" ^ (atype_to_str t) ^ "}"
    | Lbrack(t) -> "Lbrack{" ^ (atype_to_str t) ^ "}"
    | Rbrack -> "Rbrack"
    | List(t) -> "List{" ^ (atype_to_str t) ^ "}"
    | Period -> "Period"
    | Index -> "Index"
    | Let -> "Let"
    | Letvar -> "Letvar"
    | Leteq -> "Leteq"
    | Equal -> "Equal"
    | Primitive -> "Primitive"
    | Comma l -> "Comma{" ^ (List.fold_left (fun acc x -> (acc ^ (atype_to_str x) ^ " ")) "" l) ^ "}"
    | ListRparen l -> "Rparen{" ^ (List.fold_left (fun acc x -> (acc ^ (atype_to_str x) ^ " ")) "" l) ^ "}"
    | Product l -> "Product{" ^ (List.fold_left (fun acc x -> (acc ^ (atype_to_str x) ^ " ")) "" l) ^ "}"
    | Lbrace -> "Lbrace"
    | Rbrace -> "Rbrace"
    | AltLparen -> "AltLparen"
    | Plist -> "Plist"
    | AltLbrace -> "AltLbrace"
    | Code -> "Code"
    | Fun -> "Fun"
    | Funname -> "Funname"
    | Funvars -> "Funvars"
    | Closure -> "Closure"
;;

type closure = {
    plist : value;
    code : printable_type list;
    mutable state : state;
}

and value =
    | None
    | NumVal of float
    | PreOpVal of ((value * value) -> value)
    | OpVal of value * (value * value -> value)
    | ListVal of value list
    | LetVal of printable_type * (int list)
    | PrimVal of ((abstract_type * value) -> (abstract_type * value))
    | VarNameVal of printable_type
    | CodeVal of printable_type list
    | FunVal of printable_type * value
    | ClosureVal of closure

and abstract_value = abstract_type * value

and partial_state = printable_type -> abstract_value

and state = <
    push : (partial_state -> state);
    pop : partial_state;
    alter : (partial_state -> state);
    alterval : (printable_type -> abstract_value -> state);
    valuate : (printable_type -> abstract_value);
>;;

let rec value_to_str v =
    match v with
    | None -> ""
    | NumVal(f) -> Float.to_string f
    | PreOpVal(f) -> "fun"
    | OpVal(v,f) -> (value_to_str v) ^ " fun"
    | ListVal(l) -> (List.fold_left (fun acc x -> acc ^ (value_to_str x) ^ ", ") "[" l) ^ "]"
    | LetVal (v,l) -> v ^ (List.fold_left (fun acc x -> acc ^ " " ^ (string_of_int x)) "" l)
    | PrimVal _ -> "prim"
    | VarNameVal s -> s
    | CodeVal l -> List.fold_left (fun acc x -> acc ^ " " ^ x) "" l
    | FunVal (x, v) -> x ^ ", " ^ (value_to_str v)
    | ClosureVal c -> "<" ^ (value_to_str (c.plist)) ^ ", code, state>"
;;


type any_type =
    | AType of abstract_type
    | PType of printable_type
    | None
;;

let anytype_to_str t =
    match t with
    | AType s -> "AType " ^ atype_to_str s
    | PType s -> "PType " ^ s
    | None -> ""
;;

class extnum (v : int) (isinf : bool) =
    object(self)
        method value = v
        method infty = isinf
        method geq (other : extnum) : bool =
            if (self#infty && self#value >= 0) || (other#infty && other#value < 0) then true
            else if not other#infty && self#value >= other#value then true
            else false
        method str =
            if self#infty then if self#value < 0 then "-infty" else "infty"
            else string_of_int (self#value)
    end
;;

type priority = extnum;;

type token =
    | PrintableToken of printable_type * priority
    | AbstractToken of abstract_value * priority
;;

let zero = fun (n,m) -> new extnum 0 false;;
let infty = fun (n,m) -> new extnum 0 true;;
let minfty = fun (n,m) -> new extnum (-1) true;;

exception Foo of string;;

let rec index_list l i =
    match l, i with
    | ListVal [], _ -> raise (Invalid_argument "Bound out of range")
    | ListVal (t :: l), 0 -> t
    | ListVal (t :: l), i -> index_list (ListVal l) (i - 1)
;;

let unvaluelist v =
    match v with
    | ListVal l -> l
    | _ -> raise (Invalid_argument "Cannot unvalue something which is not a list")
;;

let rec swap_list (l : value) (ns : int list) (x : value) =
    match l, ns with
    | _, [] -> x
    | ListVal [], _ -> raise (Invalid_argument "Cannot give an empty list")
    | ListVal (k :: l), t :: ns -> (
        if t = 0 then
            ListVal ((swap_list k ns x) :: l)
        else
            ListVal (k :: (unvaluelist (swap_list (ListVal l) ((t - 1) :: ns) x)))
    )
;;

let let_new_state (state : state) (s : abstract_type) (x : printable_type) (v : value) (ns : int list) : partial_state =
    if ns = [] then
        fun y -> (
            if y = x then (s, v)
            else raise (Failure "not defined")
        )
    else
        let old_val = state#valuate x in
        let kappa = snd old_val in
        let new_kappa = swap_list kappa ns v in
        fun y -> (
            if y = x then ((fst old_val), new_kappa)
            else raise (Failure "not defined")
        )
;;

let create_pstate (plist : value) vals product_type =
    let rec helper plist vals product_type x = (
        match plist, vals, product_type with
        | ListVal [], _, _ -> raise (Failure "Not found")
        | ListVal ((VarNameVal y) :: plist), ListVal (v :: vals), (t :: product_type) -> if x = y then (t,v) else helper (ListVal plist) (ListVal vals) product_type x
    ) in helper plist vals product_type
;;

let rec initial_beta (first : abstract_type) (second : any_type) (state : state) :
    (abstract_type * (extnum * extnum -> extnum) * (value * value -> value * printable_type list * state)) =
    match first, second with
    | Gobble,   (AType(_) | PType(_)) -> (None, fst, fun _ -> (None, [], state))
    | Match s,  AType t -> if t = s then (None, fst, fun _ -> (None, [], state)) else raise (Invalid_argument "Matched wrong abstract type")
    | s,        AType(End) -> (s, minfty, fun (u,v) -> (u, [], state))
    | s,        AType(Op None) -> (Op(s), snd, fun (v, PreOpVal(f)) -> (OpVal(v,f), [], state))
    | Op s,     AType(Op t) -> if s = t then (Op(s), snd, (fun (OpVal(v,f),OpVal(u,g)) -> (OpVal(f(v,u),g), [], state))) else raise (Failure "s not t")
    | Op s,     AType(Rparen t) -> if s = t then (Rparen(s), snd, (fun (OpVal(v,f),u) -> (f(v,u), [], state))) else raise (Failure "s not t")
    | s,        AType(Rparen None) -> (Rparen(s), snd, fun (u,_) -> (u, [], state))
    | Lparen,   AType(Rparen s) -> (s, fst, fun (_,u) -> (u, [], state))
    (* *)
    | Lbrack(None), AType(s) -> (
        match s with
        | Lbrack _ -> raise (Failure "Cannot match lbrack with lbrack")
        | s -> (Lbrack(s), fst, fun (_,u) -> (ListVal [u], [], state))
    )
    | Lbrack(s), AType(Rbrack) -> (List(s), infty, fun (l,_) -> (l, [], state))
    | Lbrack(s), AType(t) -> if s = t then (Lbrack(s), fst, fun (ListVal l,u) -> (ListVal (l @ [u]), [], state)) else raise (Failure "Cannot match list with different type")
    | Period, AType(Num) -> (Index, zero, fun (_,n) -> n, [], state)
    | List(s), AType Index -> (s, fst, fun (l, NumVal n) -> index_list l (int_of_float n), [], state)
    (* Note changes to let stuff *)
    | Let, PType x -> (Letvar, fst, fun _ -> LetVal(x, []), [], state)
    | Letvar, AType Index -> (Letvar, fst, fun (LetVal (x,l), NumVal n) -> LetVal (x, l @ [int_of_float n]), [], state)
    | Letvar, AType Equal -> (Leteq, minfty, fun (v,_) -> v, [], state)
    | Leteq, AType s -> (None, fst, fun (LetVal (x,l), v) -> None, [], state#alter (let_new_state state s x v l))
    (* *)
    | Lbrace, None -> (None, fst, fun _ -> None, [], state#push (fun _ -> raise (Failure "End of state")))
    | Rbrace, None -> (None, fst, fun _ -> None, [], (state#pop; state))
    (* *)
    | Primitive, None -> (Match End, fst, fun (PrimVal f,_) -> let new_val = f (state#valuate "_reg_in") in (None, [], (state#alterval "_reg_out" new_val)))
    (* *)
    | s, AType(Comma []) -> (Comma [s], snd, fun (u,_) -> ListVal [u], [], state)
    | Op s, AType(Comma [t]) -> if s=t then (Comma [s], snd, fun (OpVal (u,f),ListVal[v]) -> ListVal [f(u,v)], [], state) else raise (Failure "Cannot match s and t")
    | Comma l, AType(Comma [s]) -> (Comma (l @ [s]), snd, fun (ListVal l,ListVal m) -> ListVal (l@m), [], state)
    | Comma l, AType(Rparen s) -> (ListRparen (l @ [s]), snd, fun (ListVal l,v) -> ListVal (l @ [v]), [], state)
    | Lparen, AType (ListRparen l) -> (Product l, infty, fun (_,l) -> l, [], state)
    (* *)
    | AltLparen, PType(")") -> (Plist, zero, fun (u,_) -> u, [], state)
    | AltLparen, PType("(") ->  raise (Failure "matched altparen with (")
    | AltLparen, PType(x) -> (AltLparen, snd, fun (ListVal l,_) -> ListVal (l @ [VarNameVal x]), [], state)
    | AltLparen, AType Plist -> (AltLparen, fst, fun (ListVal l, v) -> ListVal (l @ [v]), [], state)
    (* *)
    | AltLbrace, PType("}") -> (Code, infty, fun (u,_) -> u, [], state)
    | AltLbrace, PType("{") -> raise (Failure "matched altbrace with {")
    | AltLbrace, PType(x) -> (AltLbrace, infty, fun (CodeVal l,_) -> CodeVal (l @ [x]), [], state)
    | AltLbrace, AType(Code) -> (AltLbrace, infty, fun (CodeVal l, CodeVal h) -> CodeVal (l @ ("{" :: h @ ["}"])), [], state)
    (* *)
    | Fun, PType(x) -> (Funname, infty,
        let pstate y = match y with
        | "(" -> (AltLparen, ListVal [])
        | "{" -> (AltLbrace, CodeVal [])
        | _ -> raise (Failure "")
        in
        fun _ -> FunVal (x, None), [], state#push pstate)
    | Funname, AType Plist -> (Funvars, infty, fun (FunVal (x, None), v) -> FunVal (x,v), [], state)
    | Funvars, AType Code -> state#pop; (None, infty, fun (FunVal (x,v), CodeVal l) -> (
            let cval = { plist = v; code = l; state = state } in
            cval.state <- (state#alterval x (Closure, ClosureVal cval));
            (ClosureVal cval, [], state)
        ))
    | Closure, AType (Product l) -> (None, fst, fun (ClosureVal cval, k) -> (
        let pstate = create_pstate (cval.plist) k l in
        (None, cval.code @ ["}"], state#push pstate)
    ))
    | Closure, AType(x) -> (None, fst, fun (ClosureVal cval, k) -> (
        let pstate = create_pstate (cval.plist) (ListVal ([k])) [x] in
        (None, cval.code @ ["}"], state#push pstate)
    ))
    (* Very vague match, keep last *)
    | Op s,     AType(t) -> if s = t then (s, snd, (fun (OpVal(v,f),u) -> (f(v,u), [], state))) else raise (Failure "s not t")
    | End, None -> (None, fst, fun _ -> (None, [], state))
;;

let first3 = fun (a,b,c) -> a;;
let second3 = fun (a,b,c) -> b;;
let third3 = fun (a,b,c) -> c;;

let initial_priority s =
    try int_of_string s; new extnum 0 true
    with Failure _ -> (
        match s with
        | ";" -> new extnum (-1) true
        | "=" -> new extnum (-1) true
        | "(" -> new extnum 0 true
        | ")" -> new extnum 0 false
        | "+" -> new extnum 1 false
        | "*" -> new extnum 2 false
        | "-" -> new extnum 1 false
        | "/" -> new extnum 2 false
        | "[" -> new extnum 0 false
        | "]" -> new extnum 0 false
        | "." -> new extnum 0 true
        | "," -> new extnum 0 false
        | "{" -> new extnum 0 true
        | "}" -> new extnum 0 true
        | _ -> new extnum 0 true
    )
;;

let rec initial_priorities str =
    match str with 
    | [] -> []
    | s :: str -> PrintableToken(s, initial_priority s) :: (initial_priorities str)
;;

let rec print_tokens (str,state : token list * state) =
    match str with
    | [] -> print_endline ""
    | PrintableToken(t,n) :: str -> print_string (t ^ "_" ^ (n#str) ^ " "); print_tokens (str, state)
    | AbstractToken((t,v),n) :: str -> print_string ((atype_to_str t) ^ "_" ^ (n#str) ^ "(" ^ (value_to_str v) ^ ") "); print_tokens (str,state)
;;

let rec derived_beta (str, state: token list * state) : (token list * state) =
    match str with
    | [] -> [], state
    | PrintableToken(t,n) :: str -> (AbstractToken(state#valuate t, n) :: str, state)
    | AbstractToken((t,v),n) :: str -> (
        try (
            let ib = initial_beta t None state in
            let new_type = first3 ib in
            let new_priority = second3 ib (n, n) in
            let val_type_state = third3 ib (v, None) in
            let new_value = first3 val_type_state in
            let typestr = second3 val_type_state in
            let new_state = third3 val_type_state in
            if new_type = None then
                (initial_priorities typestr @ str, new_state)
            else
                ((AbstractToken((new_type, new_value), new_priority) :: initial_priorities typestr) @ str, new_state)
        ) with
        | Match_failure(_) | Failure _ -> (
            match str with
            | [] -> raise (Failure "Can't match abstract token with nothing")
            | AbstractToken((s,u),m) :: strA -> (
                if n#geq m then
                    try (
                        let ib = initial_beta t (AType(s)) state in
                        let new_type = first3 ib in
                        let new_priority = second3 ib (n,m) in
                        let val_type_state = third3 ib (v,u) in
                        let new_value = first3 val_type_state in
                        let typestr = second3 val_type_state in
                        let new_state = third3 val_type_state in
                        if new_type = None then
                            (initial_priorities typestr @ strA, new_state)
                        else
                            ((AbstractToken((new_type, new_value), new_priority) :: initial_priorities typestr) @ strA, new_state)
                    ) with
                    | Match_failure _ | Failure _ -> (
                        let next = derived_beta (str, state) in
                        let str = fst next in
                        let state = snd next in
                        AbstractToken((t,v),n) :: str, state
                    )
                else
                    let next = derived_beta (str, state) in
                    let str = fst next in
                    let state = snd next in
                    AbstractToken((t,v),n) :: str, state
            )
            | PrintableToken(s,m) :: strA -> (
                if n#geq m then
                    try (
                        let ib = initial_beta t (PType s) state in
                        let new_type = first3 ib in
                        let new_priority = second3 ib (n,m) in
                        let val_type_state = third3 ib (v,None) in
                        let new_value = first3 val_type_state in
                        let typestr = second3 val_type_state in
                        let new_state = third3 val_type_state in
                        if new_type = None then
                            (initial_priorities typestr @ strA, new_state)
                        else
                            ((AbstractToken((new_type, new_value), new_priority) :: initial_priorities typestr) @ strA, new_state)
                    ) with
                    | Match_failure _ | Failure _ -> (
                        let next = derived_beta (str, state) in
                        let str = fst next in
                        let state = snd next in
                        AbstractToken((t,v),n) :: str, state
                    )
                else
                    let next = derived_beta (str, state) in
                    let str = fst next in
                    let state = snd next in
                    AbstractToken((t,v),n) :: str, state
            )
        )
    )
;;

let rec total_beta (silent : bool) (str : token list) (state : state) =
    if not silent then print_tokens (str, state);
    let res = derived_beta (str, state) in
    let str = fst res in
    let state = snd res in
    if str != [] then
        total_beta silent str state

let initial_state = fun x ->
    try (Num, NumVal(Float.of_string x))
    with Failure _ -> (
        match x with
        | ";" -> (End, None)
        | "(" -> (Lparen, None)
        | ")" -> (Rparen None, None)
        | "+" -> (Op None, PreOpVal (fun (NumVal n, NumVal m) -> NumVal(n+.m)))
        | "*" -> (Op None, PreOpVal (fun (NumVal n, NumVal m) -> NumVal(n*.m)))
        | "-" -> (Op None, PreOpVal (fun (NumVal n, NumVal m) -> NumVal(n-.m)))
        | "/" -> (Op None, PreOpVal (fun (NumVal n, NumVal m) -> NumVal(n/.m)))
        | "[" -> (Lbrack None, None)
        | "]" -> (Rbrack, None)
        | "." -> (Period, None)
        | "let" -> (Let, None)
        | "=" -> (Equal, None)
        | "_prim_print" -> (Primitive, PrimVal (fun (a,v) -> (print_endline (value_to_str v); (None, None))))
        | "," -> (Comma [], None)
        | "{" -> (Lbrace, None)
        | "}" -> (Rbrace, None)
        | "fun" -> (Fun, None)
    )
;;

let rec valuate_stack stack (x : printable_type) : abstract_value =
    match stack with
    | [] -> raise (Failure ("value does not exist in stack"))
    | t :: stack -> (
        try t(x) with
        | Failure(_) -> valuate_stack stack x
    )
;;

let state : state = 
    object(self)
        val mutable pstate_stack = ([] : partial_state list)
        method push pstate =
            pstate_stack <- pstate :: pstate_stack;
            self
        method pop =
            match pstate_stack with
            | [] -> raise (Invalid_argument ("Empty State stack"))
            | s :: t -> pstate_stack <- t; s
        method alter pstate = (
            let top = self#pop in
            let new_top = fun x -> (
                try pstate(x) with
                | Failure(_) -> top(x)
            ) in
            self#push new_top;
            self)
        method alterval x v =
            let pstate = fun y -> if x = y then v else raise (Failure "not defined") in
            self#alter pstate
        method valuate x = valuate_stack pstate_stack x
    end
;;

state#push initial_state;;


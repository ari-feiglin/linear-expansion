type printable_type = string;;

type value =
    | None
    | NumVal of float
    | PreOpVal of ((value * value) -> value)
    | OpVal of value * (value * value -> value)
    | ListVal of value list
    | LetVal of printable_type * (int list)
;;

let rec value_to_str v =
    match v with
    | None -> ""
    | NumVal(f) -> Float.to_string f
    | PreOpVal(f) -> "fun"
    | OpVal(v,f) -> (value_to_str v) ^ " fun"
    | ListVal(l) -> (
        match l with
        | [] -> ""
        | x :: l -> (value_to_str x) ^ "," ^ (value_to_str (ListVal l))
    )
    | LetVal (v,l) -> v ^ (List.fold_left (fun acc x -> acc ^ " " ^ (string_of_int x)) "" l)
;;


type abstract_type =
    | None
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
    | Leteq
    | Equal
;;

let rec atype_to_str t =
    match t with
    | None -> "None"
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
    | Leteq -> "Leteq"
    | Equal -> "Equal"
;;

type abstract_value = abstract_type * value;;

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

type partial_state = printable_type -> abstract_value;;

let rec valuate_stack stack (x : printable_type) : abstract_value =
    match stack with
    | [] -> raise (Failure ("value does not exist in stack"))
    | t :: stack -> (
        try t(x) with
        | Failure(_) -> valuate_stack stack x
    )
;;

class state =
    object(self)
        val mutable pstate_stack = ([] : partial_state list)
        method push pstate =
            pstate_stack <- pstate :: pstate_stack
        method pop =
            match pstate_stack with
            | [] -> raise (Failure("Empty State stack"))
            | s :: t -> pstate_stack <- t; s
        method alter pstate =
            let top = self#pop in
            let new_top = fun x -> (
                try pstate(x) with
                | Failure(_) -> top(x)
            ) in
            self#push new_top;
            self
        method valuate x = valuate_stack pstate_stack x
    end
;;

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


let rec initial_beta (first : abstract_type) (second : any_type) (state : state) :
    (abstract_type * (extnum * extnum -> extnum) * (value * value -> value * printable_type list * state)) =
    match first, second with
    | s,        AType(End) -> (s, minfty, fun (u,v) -> (u, [], state))
    | s,        AType(Op None) -> (Op(s), snd, fun (v, PreOpVal(f)) -> (OpVal(v,f), [], state))
    | Op s,     AType(Op t) -> if s = t then (Op(s), snd, (fun (OpVal(v,f),OpVal(u,g)) -> (OpVal(f(v,u),g), [], state))) else raise (Failure "s not t")
    | Op s,     AType(Rparen t) -> if s = t then (Rparen(s), snd, (fun (OpVal(v,f),u) -> (f(v,u), [], state))) else raise (Failure "s not t")
    | s,        AType(Rparen None) -> (Rparen(s), snd, fun (u,_) -> (u, [], state))
    | Lparen,   AType(Rparen s) -> (s, fst, fun (_,u) -> (u, [], state))
    (* Very vague match, keep last *)
    | Op s,     AType(t) -> if s = t then (s, snd, (fun (OpVal(v,f),u) -> (f(v,u), [], state))) else raise (Failure "s not t")
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
    | Let, PType x -> (
        match x with
        | "=" -> (Leteq, minfty, fun (x,_) -> x, [], state)
        | "." -> raise (Failure "Cannot match let with .")
        | _ ->(Let, snd, fun (_,_) -> LetVal (x, []), [], state)
    )
    | Let, AType Index -> (Let, fst, fun (LetVal (x,l), NumVal n) -> LetVal (x, l @ [int_of_float n]), [], state)
    | Leteq, AType s -> (None, fst, fun (LetVal (x,l), v) -> None, [], state#alter (let_new_state state s x v l))
;;

let first3 = fun (a,b,c) -> a;;
let second3 = fun (a,b,c) -> b;;
let third3 = fun (a,b,c) -> c;;

let initial_priority s =
    try int_of_string s; new extnum 0 true
    with Failure _ -> (
        match s with
        | ";" -> new extnum 0 false
        | "(" -> new extnum 0 true
        | ")" -> new extnum 0 false
        | "+" -> new extnum 1 false
        | "*" -> new extnum 2 false
        | "-" -> new extnum 1 false
        | "/" -> new extnum 2 false
        | "[" -> new extnum 0 false
        | "]" -> new extnum 0 false
        | "." -> new extnum 0 true
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

let rec total_beta (str : token list) (state : state) =
    print_tokens (str, state);
    let res = derived_beta (str, state) in
    let str = fst res in
    let state = snd res in
    if str != [] then
        total_beta str state

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
    )
;;

let state = new state;;
state#push initial_state;;


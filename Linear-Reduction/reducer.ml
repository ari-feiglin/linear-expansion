type printable_term = string;;

type value =
    | None
    | NumVal of float
    | PreOpVal of ((value * value) -> value)
    | OpVal of value * (value * value -> value)
    | ListVal of value list
    | LetVal of printable_term * (int list)
    | PrimVal of ((type_term * value) -> (type_term * value))
    | VarNameVal of printable_term
    | CodeVal of printable_term list
    | FunVal of printable_term * value
    | ClosureVal of closure

and closure = {
    plist : value;
    code : printable_term list;
    mutable state : partial_state;
}

and partial_state = printable_term -> pi_i

and type_term =
    | None
    | Num
    | List of type_term
    | Closure
    | Product of (type_term list)
    | String
    | Primitive

and abstract_term =
    | None
    | Gobble
    | Match of type_term
    | End
    | Op of type_term
    | Lparen
    | Rparen of type_term
    | Lbrack of type_term
    | Rbrack
    | Period
    | Index
    | Let
    | Letvar
    | Leteq
    | Equal
    | Comma of (type_term list)
    | Listrparen of (type_term list)
    | Lbrace
    | Rbrace
    | AltLparen
    | AltLbrace
    | Code
    | Fun
    | Funname
    | Funvars
    | Plist

and term =
    | None
    | PTerm of printable_term
    | TTerm of type_term
    | ATerm of abstract_term

and term_i =
    | None
    | TTerm of type_term
    | ATerm of abstract_term

and pi_i = term_i * value

and pi =
    | Pi_I of pi_i
    | PTerm of printable_term
;;

let rec tterm_to_str (t : type_term) =
    match t with
    | None -> "None"
    | Num -> "Num"
    | List(t) -> "List{" ^ (tterm_to_str t) ^ "}"
    | Product l -> "Product{" ^ (List.fold_left (fun acc x -> (acc ^ (tterm_to_str x) ^ " ")) "" l) ^ "}"
    | Closure -> "Closure"
    | Primitive -> "Primitive"
;;


let aterm_to_str (t : abstract_term) =
    match t with
    | None -> "None"
    | Gobble -> "Gobble"
    | Match(t) -> "Match{" ^ (tterm_to_str t) ^ "}"
    | End -> "End"
    | Op(t) -> "Op{" ^ (tterm_to_str t) ^ "}"
    | Lparen -> "Lparen"
    | Rparen(t) -> "Rparen{" ^ (tterm_to_str t) ^ "}"
    | Lbrack(t) -> "Lbrack{" ^ (tterm_to_str t) ^ "}"
    | Rbrack -> "Rbrack"
    | Period -> "Period"
    | Index -> "Index"
    | Let -> "Let"
    | Letvar -> "Letvar"
    | Leteq -> "Leteq"
    | Equal -> "Equal"
    | Comma l -> "Comma{" ^ (List.fold_left (fun acc x -> (acc ^ (tterm_to_str x) ^ " ")) "" l) ^ "}"
    | Listrparen l -> "Rparen{" ^ (List.fold_left (fun acc x -> (acc ^ (tterm_to_str x) ^ " ")) "" l) ^ "}"
    | Lbrace -> "Lbrace"
    | Rbrace -> "Rbrace"
    | AltLparen -> "AltLparen"
    | AltLbrace -> "AltLbrace"
    | Code -> "Code"
    | Fun -> "Fun"
    | Funname -> "Funname"
    | Funvars -> "Funvars"
    | Plist -> "Plist"
;;

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
        | "_prim_print" -> new extnum (-1) true
        | _ -> new extnum 0 true
    )
;;

let rec initial_priorities (str : string list) : ((pi * priority) list) =
    match str with 
    | [] -> []
    | s :: str -> (PTerm s, initial_priority s) :: (initial_priorities str)
;;

let rec valuate_stack (stack : partial_state list) (x : printable_term) : pi_i =
    match stack with
    | [] -> raise (Failure ("printable term " ^ x ^ " does not exist in stack"))
    | t :: stack -> (
        try t x with
        | Failure _ -> valuate_stack stack x
    )
;;

class state =
    object(self)
        val mutable pstate_stack = ([] : partial_state list)
        val mutable closure_stack = ([] : int list)
        val mutable depth = 0
        method push pstate =
            pstate_stack <- pstate :: pstate_stack;
            depth <- depth + 1;
            self
        method push_closure pstate =
            closure_stack <- depth :: closure_stack;
            self#push pstate
        method pop = (
            (match pstate_stack with
            | [] -> raise (Invalid_argument "Empty state stack")
            | ps :: stack -> (pstate_stack <- stack;
                pstate_stack <- stack;
                depth <- depth - 1;
            )
            );
            (try (if List.hd closure_stack = depth then
                closure_stack <- List.tl closure_stack)
            with
            | Failure _ -> ());
            self
        )
        method set_top pstate =
            pstate_stack <- pstate :: (List.tl pstate_stack)
        method alter pstate =
            let top = List.hd pstate_stack in
            let new_top = fun x -> (
                try pstate x with
                | Failure _ -> top x
            ) in
            self#set_top new_top;
            self
        method alter_var x v =
            let pstate = fun y -> if x = y then v else raise (Failure "Not defined") in
            self#alter pstate
        method valuate x =
            valuate_stack pstate_stack x
    end
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

let let_new_state (state : state) (s : term_i) (x : printable_term) (v : value) (ns : int list) : partial_state =
    if s = None then raise (Failure "Cannot put None value in state")
    else if ns = [] then
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

let term_i_to_term (t : term_i) : term =
    match t with
    | None -> None
    | TTerm s -> TTerm s
    | ATerm s -> ATerm s
;;

let term_to_term_i (t : term) : term_i =
    match t with
    | None -> None
    | TTerm s -> TTerm s
    | ATerm s -> ATerm s
    | PTerm s -> raise (Failure "Cannot convert printable term to term_i")
;;

let zero = fun (n,m) -> new extnum 0 false;;
let infty = fun (n,m) -> new extnum 0 true;;
let minfty = fun (n,m) -> new extnum (-1) true;;

let rec initial_beta (first : term_i) (second : term) :
    (term_i * (extnum * extnum -> extnum) * (value * value * state -> value * (printable_term list) * state)) =
    match first, second with
    | sigma, ATerm End -> (sigma, minfty, fun (u,_,s) -> (u,[],s))
    (* Operators *)
    | TTerm sigma, ATerm (Op None) -> (ATerm (Op sigma), snd, fun (u,PreOpVal f,s) -> (OpVal(u,f),[],s))
    | ATerm (Op sigma), ATerm (Op tau) -> (
        if sigma = tau then
            (ATerm (Op sigma), snd, fun (OpVal(u,f), OpVal(v,g), s) -> (OpVal(f(u,v),g), [], s))
        else
            raise (Failure "Cannot match operators with different types")
    )
    | ATerm (Op sigma), TTerm tau -> (
        if sigma = tau then
            (TTerm sigma, snd, fun (OpVal(u,f), v, s) -> (f(u,v), [], s))
        else
            raise (Failure "Cannot match operator with different type")
    )
    (* Parentheses *)
    | TTerm sigma, ATerm (Rparen None) -> (ATerm (Rparen sigma), snd, fun (u,_,s) -> (u,[],s))
    | ATerm (Op sigma), ATerm (Rparen tau) -> (
        if sigma = tau then
            (ATerm (Rparen sigma), snd, fun (OpVal(u,f), v, s) -> (f(u,v), [], s))
        else
            raise (Failure "Cannot match operator with rparen of different type")
    )
    | ATerm Lparen, ATerm (Rparen sigma) -> (TTerm sigma, fst, fun (_,u,s) -> (u,[],s))
    (* Lists *)
    | ATerm (Lbrack None), TTerm sigma -> (ATerm (Lbrack sigma), fst, fun (_,u,s) -> (ListVal [u],[],s))
    | ATerm (Lbrack sigma), TTerm tau -> (
        if sigma = tau then
            (ATerm (Lbrack sigma), fst, fun (ListVal l,u,s) -> (ListVal (l @ [u]), [], s))
        else
            raise (Failure "Cannot match list building element with different type")
    )
    | ATerm (Lbrack sigma), ATerm Rbrack -> (TTerm (List sigma), infty, fun (l,_,s) -> (l, [], s))
    | ATerm Period, TTerm Num -> (ATerm Index, zero, fun (_, n, s) -> (n, [], s))
    | TTerm (List sigma), ATerm Index -> (TTerm sigma, fst, fun (ListVal l, NumVal i, s) -> (List.nth l (int_of_float i), [], s))
    (* Variables *)
    | ATerm Let, PTerm x -> (ATerm Letvar, snd, fun (_,_,s) -> (LetVal (x,[]), [], s))
    | ATerm Letvar, ATerm Index -> (ATerm Letvar, fst, fun (LetVal (x,l), NumVal n, s) -> (LetVal (x, l@[int_of_float n]), [], s))
    | ATerm Letvar, ATerm Equal -> (ATerm Leteq, minfty, fun (u,_,s) -> (u, [], s))
    | ATerm Leteq, sigma -> (None, fst, fun (LetVal (x,l), u, s) -> None, [], s#alter (let_new_state s (term_to_term_i sigma) x u l))
    (* Scoping *)
    | ATerm Lbrace, None -> (None, fst, fun (_,_,s) -> (None, [], s#push (fun x -> raise (Failure "Empty state"))))
    | ATerm Rbrace, None -> (None, fst, fun (_,_,s) -> (None, [], s#pop))
    (* Products *)
    | TTerm sigma, ATerm (Comma []) -> (ATerm (Comma [sigma]), snd, fun (u,_,s) -> (ListVal [u], [], s))
    | ATerm (Op sigma), ATerm (Comma [tau]) -> (
        if sigma = tau then
            (ATerm (Comma [sigma]), snd, fun (OpVal (u,f), ListVal [v], s) -> (ListVal [f(u,v)], [], s))
        else
            raise (Failure "Cannot match operator of one type with comma of another")
    )
    | ATerm (Comma omega), ATerm (Comma [sigma]) -> (ATerm (Comma (omega @ [sigma])), snd, fun (ListVal l1, ListVal l2, s) -> (ListVal (l1 @ l2), [], s))
    | ATerm (Comma omega), ATerm (Rparen sigma) -> (ATerm (Listrparen (omega @ [sigma])), snd, fun (ListVal l, v, s) -> (ListVal (l @ [v]), [], s))
    | ATerm Lparen, ATerm (Listrparen omega) -> (TTerm (Product omega), infty, fun (_, ListVal l, s) -> (ListVal l, [], s))
    (* Primitives *)
    | TTerm Primitive, TTerm sigma -> (
        (None, fst, fun (PrimVal f, v, s) -> (let res = f (sigma, v) in let t_i = fst res in let value = snd res in (None, [], (s#alter_var "_reg_out" (TTerm t_i, value)))))
    )
    (* Code Capture *)
    | ATerm AltLbrace, PTerm "}" -> (ATerm Code, infty, fun (xi,_,s) -> (xi, [], s))
    | ATerm AltLbrace, PTerm "{" -> raise (Failure "Cannot match { with {")
    | ATerm AltLbrace, PTerm x -> (ATerm AltLbrace, infty, fun (CodeVal xi,_,s) -> (CodeVal (xi @ [x]), [], s))
    | ATerm AltLbrace, ATerm Code -> (ATerm AltLbrace, infty, fun (CodeVal xi1, CodeVal xi2, s) -> (CodeVal (xi1 @ ["{"] @ xi2 @ ["}"]), [], s))
    (* Parameter Capture *)
    | ATerm AltLparen, PTerm ")" -> (ATerm Plist, fst, fun (l,_,s) -> (l,[],s))
    | ATerm AltLparen, PTerm "(" -> raise (Failure "Cannot match ( with (")
    | ATerm AltLparen, PTerm "," -> (ATerm AltLparen, fst, fun (l,_,s) -> (l, [], s))
    | ATerm AltLparen, PTerm x -> (ATerm AltLparen, fst, fun (ListVal l,_,s) -> (ListVal (l @ [VarNameVal x]), [], s))
    | ATerm AltLparen, ATerm Plist -> (ATerm AltLparen, fst, fun (ListVal l,u,s) -> (ListVal (l @ [u]), [], s))
;;

type pi_priority = pi * priority;;

let first3 (x,y,z) = x;;
let second3 (x,y,z) = y;;
let third3 (x,y,z) = z;;

let try_initial (first : term_i) (first_val : value) (first_priority : priority) (second : term) (second_val : value) (second_priority : priority) (tokens : pi_priority list) (state : state) =
    let ib = initial_beta first second in
    let new_term = first3 ib in
    let priority_function = second3 ib in
    let value_function = third3 ib in
    let new_priority = priority_function (first_priority, second_priority) in
    let new_values = value_function (first_val, second_val, state) in
    let new_value = first3 new_values in
    let printable_string = second3 new_values in
    let new_state = third3 new_values in
    if new_term = None then
        (initial_priorities printable_string @ tokens, new_state)
    else
        (((Pi_I (new_term, new_value), new_priority) :: (initial_priorities printable_string)) @ tokens, new_state)
;;

let rec derived_beta (tokens, state : pi_priority list * state) : (pi_priority list * state) =
    match tokens with
    | [] -> [], state
    | (PTerm x, n) :: tokens -> ((Pi_I (state#valuate x), n) :: tokens, state)
    | (Pi_I (t,u), n) :: toks -> (
        try try_initial t u n None None n toks state with
        | Match_failure _ | Failure _ -> (
            match toks with
            | [] -> raise (Failure "Can't match internal term with nothing")
            | (Pi_I (s,v), m) :: toksA -> (
                if n#geq m then
                    try try_initial t u n (term_i_to_term s) v m toksA state with
                    | Match_failure _ | Failure _ -> (
                        let next = derived_beta (toks, state) in
                        let toks = fst next in
                        let state = snd next in
                        ((Pi_I (t, u), n) :: toks, state)
                    )
                else 
                    let next = derived_beta (toks, state) in
                    let toks = fst next in
                    let state = snd next in
                    ((Pi_I (t, u), n) :: toks, state)
            )
            | (PTerm x, m) :: toksA -> (
                if n#geq m then
                    try try_initial t u n (PTerm x) None m toksA state with
                    | Match_failure _ | Failure _ -> (
                        let next = derived_beta (toks, state) in
                        let toks = fst next in
                        let state = snd next in
                        ((Pi_I (t, u), n) :: toks, state)
                    )
                else
                    let next = derived_beta (toks, state) in
                    let toks = fst next in
                    let state = snd next in
                    ((Pi_I (t, u), n) :: toks, state)
            )
        )
    )
;;

let rec print_tokens (str,state : pi_priority list * state) =
    match str with
    | [] -> print_endline ""
    | (PTerm t, n) :: str -> print_string (t ^ "_" ^ (n#str) ^ " "); print_tokens (str, state)
    | (Pi_I (TTerm t, v), n) :: str -> print_string ((tterm_to_str t) ^ "_" ^ (n#str) ^ "(" ^ (value_to_str v) ^ ") "); print_tokens (str,state)
    | (Pi_I (ATerm t, v), n) :: str -> print_string ((aterm_to_str t) ^ "_" ^ (n#str) ^ "(" ^ (value_to_str v) ^ ") "); print_tokens (str,state)
    | (Pi_I (None, v), n) :: str -> print_string "None "; print_tokens (str, state)
;;

let rec total_beta (silent : bool) (str : pi_priority list) (state : state) =
    if not silent then print_tokens (str, state);
    let res = derived_beta (str, state) in
    let str = fst res in
    let state = snd res in
    if str != [] then
        total_beta silent str state

let initial_state (x : printable_term) : pi_i =
    try (TTerm Num, NumVal(Float.of_string x))
    with Failure _ -> (
        match x with
        | ";" -> (ATerm End, None)
        | "(" -> (ATerm Lparen, None)
        | ")" -> (ATerm (Rparen None), None)
        | "+" -> (ATerm (Op None), PreOpVal (fun (NumVal n, NumVal m) -> NumVal(n+.m)))
        | "*" -> (ATerm (Op None), PreOpVal (fun (NumVal n, NumVal m) -> NumVal(n*.m)))
        | "-" -> (ATerm (Op None), PreOpVal (fun (NumVal n, NumVal m) -> NumVal(n-.m)))
        | "/" -> (ATerm (Op None), PreOpVal (fun (NumVal n, NumVal m) -> NumVal(n/.m)))
        | "[" -> (ATerm (Lbrack None), None)
        | "]" -> (ATerm Rbrack, None)
        | "." -> (ATerm Period, None)
        | "let" -> (ATerm Let, None)
        | "=" -> (ATerm Equal, None)
        | "_prim_print" -> (TTerm Primitive, PrimVal (fun (a,v) -> (print_endline (value_to_str v); (None, None))))
        | "," -> (ATerm (Comma []), None)
        | "{" -> (ATerm Lbrace, None)
        | "}" -> (ATerm Rbrace, None)
        | "fun" -> (ATerm Fun, None)
        | _ -> raise (Failure "Printable term not found in state")
    )
;;

let state = new state;;
state#push initial_state;;


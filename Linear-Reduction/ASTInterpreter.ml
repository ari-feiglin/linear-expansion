type expr =
  | Num of int
  | Bool of bool
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Lt of expr * expr
  | Gt of expr * expr
  | Eq of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Fun of string * expr
  | App of expr * expr
  | Loop of expr * expr

type value =
  | NumVal of int
  | BoolVal of bool
  | FunVal of string * expr * (string * value) list ref

let rec eval (env : (string * value) list ref) (e : expr) : value =
  match e with
  | Num n -> NumVal n
  | Bool b -> BoolVal b
  | Var x -> List.assoc x !env
  | Add (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | NumVal n1, NumVal n2 -> NumVal (n1 + n2)
       | _ -> failwith "Type error: addition expects numbers")
  | Sub (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | NumVal n1, NumVal n2 -> NumVal (n1 - n2)
       | _ -> failwith "Type error: subtraction expects numbers")
  | Mul (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | NumVal n1, NumVal n2 -> NumVal (n1 * n2)
       | _ -> failwith "Type error: multiplication expects numbers")
  | Div (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | NumVal n1, NumVal n2 -> NumVal (n1 / n2)
       | _ -> failwith "Type error: division expects numbers")
  | Lt (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | NumVal n1, NumVal n2 -> BoolVal (n1 < n2)
       | _ -> failwith "Type error: less than expects numbers")
  | Gt (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | NumVal n1, NumVal n2 -> BoolVal (n1 > n2)
       | _ -> failwith "Type error: greater than expects numbers")
  | Eq (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | NumVal n1, NumVal n2 -> BoolVal (n1 = n2)
       | BoolVal b1, BoolVal b2 -> BoolVal (b1 = b2)
       | _ -> failwith "Type error: equality expects numbers or booleans")
  | And (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | BoolVal b1, BoolVal b2 -> BoolVal (b1 && b2)
       | _ -> failwith "Type error: logical AND expects booleans")
  | Or (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | BoolVal b1, BoolVal b2 -> BoolVal (b1 || b2)
       | _ -> failwith "Type error: logical OR expects booleans")
  | Not e ->
      let v = eval env e in
      (match v with
       | BoolVal b -> BoolVal (not b)
       | _ -> failwith "Type error: logical NOT expects a boolean")
  | If (e1, e2, e3) ->
      let v1 = eval env e1 in
      (match v1 with
       | BoolVal true -> eval env e2
       | BoolVal false -> eval env e3
       | _ -> failwith "Type error: if condition expects a boolean")
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let new_env = (x, v1) :: !env in
      eval (ref new_env) e2
  | Fun (x, e) -> FunVal (x, e, env)
  | App (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1 with
       | FunVal (x, body, closure_env) ->
           let new_env = (x, v2) :: !closure_env in
           eval (ref new_env) body
       | _ -> failwith "Type error: application expects a function")
  | Loop (e1, e2) ->
      let rec loop () =
        let v1 = eval env e1 in
        match v1 with
        | BoolVal true -> let _ = eval env e2 in loop ()
        | BoolVal false -> NumVal 0
        | _ -> failwith "Type error: loop condition expects a boolean"
      in
      loop ()

let run e =
  let start = Sys.time () in
  let result = eval (ref []) e in
  let stop = Sys.time () in
  Printf.printf "Execution time for the expression: %f seconds\n" (stop -. start);
  result

(* Example usage *)
(* Define the expressions *)
let e1 = Mul (Add (Num 2, Num 3), Sub (Num 5, Num 1))
let e2 = Div (Mul (Num 4, Num 5), Add (Num 3, Num 2))
let e3 = Lt (e1, e2)
let e4 = If (e3, e1, e2)
let e5 = Let ("x", Sub (Num 10, Num 3), Add (Var "x", Num 5))
let e6 = Fun ("x", Add (Var "x", Num 1))
let e7 = App (e6, Num 7)

let e10 =
  let rec make_expr depth =
    if depth <= 0 then
      Num 1
    else
      Add (make_expr (depth - 1), Mul (Num 2, make_expr (depth - 1)))
  in
  make_expr 20;;

(* Print the results *)
print_endline (string_of_int (match run e1 with NumVal n -> n | _ -> failwith "Expected a number"));
print_endline (string_of_int (match run e2 with NumVal n -> n | _ -> failwith "Expected a number"));
print_endline (string_of_bool (match run e3 with BoolVal b -> b | _ -> failwith "Expected a boolean"));
print_endline (string_of_int (match run e4 with NumVal n -> n | _ -> failwith "Expected a number"));
print_endline (string_of_int (match run e5 with NumVal n -> n | _ -> failwith "Expected a number"));
print_endline (string_of_int (match run e7 with NumVal n -> n | _ -> failwith "Expected a number"));

print_endline "Starting evaluation of e10...";
let _ = run e10 in
print_endline "Evaluation of e10 completed.";

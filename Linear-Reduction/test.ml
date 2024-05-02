type value =
    | ListVal of value list
    | IntVal of int
;;

let unvaluelist v =
    match v with
    | ListVal l -> l
    | _ -> raise (Invalid_argument "Cannot unvalue something which is not a list")
;;

let rec swap_list (l : value) (ns : int list) (x : value) =
    match l, ns with
    | ListVal [], _ -> raise (Invalid_argument "Cannot give an empty list")
    | _, [] -> x
    | ListVal (k :: l), t :: ns -> (
        if t = 0 then
            ListVal ((swap_list k ns x) :: l)
        else
            ListVal (k :: (unvaluelist (swap_list (ListVal l) ((t - 1) :: ns) x)))
    )
;;

let test = swap_list (ListVal [ListVal [IntVal 1; IntVal 2]; ListVal [IntVal 1; IntVal 2;]]) [0; 1] (IntVal 3);;
print_endline "Done";;


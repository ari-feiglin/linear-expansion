open Lexer
open Reducer

let tokens = get_all_tokens ("(1 + 2) * 3;");;

(*let tokens = get_all_tokens "
    let x = 1;;
";;*)

if Array.length Sys.argv > 1 && Sys.argv.(1) = "y" then
    total_beta true (initial_priorities tokens) state
else
    total_beta false (initial_priorities tokens) state
;;


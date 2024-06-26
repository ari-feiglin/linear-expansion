open Lexer
open Reducer

let tokens = get_all_tokens ("
fun fib (n) {
    if
    | n == 0 -> {1}
    | n == 1 -> {1}
    | 1 -> {fib(n-1) + fib(n-2)}
    fi
}

_prim_print fib(5);
");;

(*let tokens = get_all_tokens "
    let x = 1;;
";;*)

if Array.length Sys.argv > 1 && Sys.argv.(1) = "y" then
    total_beta true (initial_priorities tokens) state
else
    total_beta false (initial_priorities tokens) state
;;


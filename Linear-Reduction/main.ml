open Lexer
open Reducer

let tokens = get_all_tokens ("
fun fib (n) {
    if n {
        if (n - 1) {
            (fib (n-1) + fib (n-2))
        }{1}
    }{1}
}
let x = fib (3);
_prim_print x;
");;

(*let tokens = get_all_tokens "
    let x = 1;;
";;*)

if Array.length Sys.argv > 1 && Sys.argv.(1) = "y" then
    total_beta true (initial_priorities tokens) state
else
    total_beta false (initial_priorities tokens) state
;;


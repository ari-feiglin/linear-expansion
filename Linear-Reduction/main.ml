open Lexer
open Reducer

let tokens = get_all_tokens ("
    let _reg_in = 1;
    {
    let _reg_in = ((1 + 2) * 3,2);
    print;
    }
    print;
");;

total_beta (initial_priorities tokens) state;;

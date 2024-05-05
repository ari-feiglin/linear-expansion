open Lexer
open Reducer

let tokens = get_all_tokens ("
    let x = [1; 2;];
    let y = [x.1; x.0;];
    let y.0 = 0;
    let x.(y.0) = 2;
    let _reg_in = x;
    print;
    let _reg_in = 1;
    print;
");;

total_beta (initial_priorities tokens) state;;

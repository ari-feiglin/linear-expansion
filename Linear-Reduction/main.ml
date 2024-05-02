open Lexer
open Reducer

let tokens = get_all_tokens ("
    let x = [1; 2;];
    let x.0 = 0;
    let y = [x.1; x.0;];
    [y.0; y.1;].(x.0 + [0; 0; 1;].2);
");;

total_beta (initial_priorities tokens) state;;

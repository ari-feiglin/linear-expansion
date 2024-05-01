open Lexer
open Reducer

let tokens = get_all_tokens "[1;2;3 + 6 * ([0;1;2;].2);].(1+1) * 3;";;
total_beta (initial_priorities tokens) state;;

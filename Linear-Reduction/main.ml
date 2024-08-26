open Lexer
open Reducer

let rec read_chan chan =
    try (
        let curr_line = (input_line chan) ^ "\n" in
        let rest = read_chan chan in
        curr_line ^ rest
    ) with
    | End_of_file -> (
        close_in chan;
        ""
    )
;;

let read_file filename =
    let chan = open_in filename in
    read_chan chan
;;

let () =
    if Array.length Sys.argv < 2 then (
        print_endline "Must supply filename";
        exit 1;
    );

    if Array.length Sys.argv > 2 && Sys.argv.(2) = "y" then
        parameters.silent <- true;

    let tokens = Sys.argv.(1) |> read_file |> get_all_tokens in
    total_beta (initial_priorities tokens) state;
    ()
;;


type linked = {
    mutable value : int;
    mutable next : linked option;
}

let l : linked = {value = 10; next = None};;

l.next <- Some l;;

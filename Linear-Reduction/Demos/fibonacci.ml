fun print (x) {
    _prim_print x;
}

fun fibonacci (n) {
    switch
    | n == 0 -> { [0;] }
    | n == 1 -> { [0; 1;] }
    | 1 -> {
        let prev = fibonacci (n-1);
        prev @ [prev.(n-1) + prev.(n-2);]
    }
    end
}

print (fibonacci 30);


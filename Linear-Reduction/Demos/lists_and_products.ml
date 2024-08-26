fun print (x) {
    _prim_print x;
}

fun length (l) {
    _prim_len l
    _reg_out
}

fun tail (l) {
    _prim_tail l
    _reg_out
}

fun map (l,f) {
    if (length l == 0) {
        l
    }{
        [f (l.0);] @ (map ((tail l), f))
    }
}

fun square (n) {
    n * n
}

let arr = [0-3; 0-2; 0-1; 0; 1; 2; 3;];
print (map (arr, square));

fun fold_left (f,acc,l) {
    if (length l == 0) {
        acc
    }{
        fold_left (f, f (acc,l.0), tail l)
    }
}

fun sub (n,m) {
    n - m
}

let arr = [1; 2; 3; 4;];
print (fold_left (sub, 10, arr));

fun reverse (l) {
    if (length l == 0) {
        l
    }{
        reverse (tail l) @ [l.0;]
    }
}

print (reverse arr);

fun fst (a,b) {
    a
}

fun snd (a,b) {
    b
}

fun flip (p) {
    (snd p, fst p)
}

print (flip (10,20));


fun print (x) {
    _prim_print x;
}

fun curry (f) {
    fun curried (x) {
        fun curriedX (y) {
            f (x,y)
        }
        curriedX
    }
    curried
}

fun plus (x,y) {
    x + y
}

print (plus (10, 20));
let curry_plus = curry plus;
print (curry_plus 10 20);


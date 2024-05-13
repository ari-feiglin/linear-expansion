type state = < push : (int -> state) >

let x : state =
    object(self)
        val mutable stack = ([] : int list)
        method push v =
            stack <- v :: stack;
            self
    end
;;

x#push 0;;


(* Programming Languages, Dan Grossman *)
(* Section 2: Tail Recursion *)

fun fact1 n = if n=0 then 1 else n * fact1(n-1)

fun fact2 n =
    let fun aux(n,acc) = if n=0 then acc else aux(n-1,acc*n)
    in
        aux(n,1)
    end

fun fact3 n =
    let
        fun aux (0,acc) = acc
          | aux (n,acc) = aux (n-1,acc*n)
    in
        aux(n,1)
    end

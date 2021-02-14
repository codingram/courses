(* Programming Languages, Dan Grossman *)
(* Section 3: Closures and Recomputation *)

(* NOTE: Use of ';' and 'print' function:
 * Whenver there is a semicolon between two expressions like e1; e2, the
 * result of the first expression is thrown away and the result of the
 * second expression is kept. Useful for putting print statements to either
 * understand the code or debug the code as shown below. *)

fun filter (f,xs) =
    case xs of
	[] => []
      | x::xs' => if f x then x::(filter(f,xs')) else filter(f,xs')

fun allShorterThan1 (xs,s) = 
    filter (fn x => String.size x < (print "!"; String.size s), xs)

fun allShorterThan2 (xs,s) =
    let 
	val i = (print "!"; String.size s)
    in
	filter(fn x => String.size x < i, xs)
    end

val _ = print "\nwithAllShorterThan1: "

val x1 = allShorterThan1(["1","333","22","4444"],"xxx")

val _ = print "\nwithAllShorterThan2: "

val x2 = allShorterThan2(["1","333","22","4444"],"xxx")

val _ = print "\n"

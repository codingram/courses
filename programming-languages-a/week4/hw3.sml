(* Coursera Programming Languages, Homework 3 *)
(* You can obtain the "function" corresponding to an infix operator via op, like so:
 * (op <>) is the function fn (x,y) => x <> y *)

exception NoAnswer


(* Returns a list of string which starts with an uppercase letter.
 * Assumption: All string have at least 1 character. *)
fun only_capitals strings =
    List.filter (fn elem => Char.isUpper (String.sub (elem, 0))) strings


(* Returns the longest string from the given list of strings.
 * If the list is empty, return an empty string "" and in case of a tie,
 * return the string closest to the beginning of the list *)
fun longest_string1 strings =
    foldl (fn (elem, longest) =>
              if String.size elem > String.size longest then elem else longest)
          ""
          strings


(* Same as that of longest_string1 except in case of tie, it returns the string
 * which is closest to the end of the list. *)
fun longest_string2 strings =
    foldl (fn (elem, longest) =>
              if String.size elem >= String.size longest then elem else longest)
          ""
          strings


(* A more general version of the above two functions which takes in a function and if
 * it behaves like > (so it returns true exactly when its first argument is strictly
 * greater than its second), then the returned function has the same behavior as
 * longest_string1. *)
fun longest_string_helper func strings =
    foldl (fn (elem, longest) =>
              if func (String.size elem, String.size longest) then elem else longest)
          ""
          strings


(* Same as longest_string1 *)
val longest_string3 = longest_string_helper (op >)


(* Same as longest_string2 *)
val longest_string4 = longest_string_helper (op >=)


(* Returns the longest string in the list that begins with an uppercase letter,
 * or "" if there are no such strings. *)
val longest_capitalized = longest_string1 o only_capitals


(* Returns the given string in reverse order. *)
val rev_string = String.implode o List.rev o String.explode


(* Given function is applied to the elements of the given list in order and
 * returns the answer 'v' if the function returns SOME v. If all the elements
 * returns NONE, NoAnswer exception will be raised. *)
fun first_answer func [] = raise NoAnswer
  | first_answer func (a::rest) = case func a of
                                      NONE => first_answer func rest
                                    | SOME ans => ans


(* Return SOME lst where lst contains the elements obtained from applying
 * the given function to each element of the given list. If any of the element
 * returned NONE, the function will return NONE. *)
fun all_answers func lst =
    let
        fun loop ([], acc) = SOME acc
          | loop (a::rest, acc) = case func a of
                                      NONE => NONE
                                    | SOME ans => loop(rest, acc @ ans)
    in
        loop (lst, [])
    end


(* Necessary information for further problems *)

datatype pattern = Wildcard
                 | Variable of string
                 | UnitP
                 | ConstP of int
                 | TupleP of pattern list
                 | ConstructorP of string * pattern

datatype valu = Const of int
              | Unit
              | Tuple of valu list
              | Constructor of string * valu

fun g f1 f2 p =
    let
        val r = g f1 f2
    in
        case p of
             Wildcard          => f1 ()
           | Variable x        => f2 x
           | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
           | ConstructorP(_,p) => r p
           | _                 => 0
    end


(* Returns the count of Wildcard patterns in the given pattern. *)
fun count_wildcards p =
    g (fn () => 1) (fn _ => 0) p


(* Returns the total number of Wildcard patterns and the sum of string lengths
 * of all the variables in the variable patterns from the given pattern. *)
fun count_wild_and_variable_lengths p =
    count_wildcards p + g (fn () => 0) String.size p


(* Return the number of times the given string appears as a variable in the pattern. *)
fun count_some_var (s, p) =
    g (fn () => 0) (fn v => if s = v then 1 else 0) p


(* Returns true if and only if all the variables appearing in the pattern are distinct
 * from each other. Return true if there are no variables in the pattern. *)
fun check_pat p =
    let
        fun collect_vars (Variable v) = [v]
          | collect_vars (TupleP ps) =
            List.foldl (fn (p, vs) => (collect_vars p) @ vs) [] ps
          | collect_vars (ConstructorP (_, p)) = collect_vars p
          | collect_vars _ = []

        fun all_distinct [] = true
          | all_distinct [_] = true
          | all_distinct (a::rest) =
            not (List.exists (fn b => a = b) rest)
            andalso all_distinct rest
    in
        all_distinct (collect_vars p)
    end


(* Returns SOME lst where lst is the list of bindings if the pattern matches,
 * otherwise NONE. *)
fun match (_, Wildcard) = SOME []
  | match (v, Variable s) = SOME [(s, v)]
  | match (Unit, UnitP) = SOME []
  | match (Const c1, ConstP c2) = if c1 = c2 then SOME [] else NONE
  | match (Tuple vs, TupleP ps) =
    (all_answers match (ListPair.zipEq (vs, ps)) handle UnequalLenghts => NONE)
  | match (Constructor (s2, v), ConstructorP (s1, p)) =
    if s1 = s2 then match (v, p) else NONE
  | match _ = NONE


(* Returns SOME lst where lst is the list of bindings for the first pattern
 * in the given list that matches, NONE if no pattern matches. *)
fun first_match v ps =
    SOME (first_answer (fn p => match (v, p)) ps) handle NoAnswer => NONE


(* --------------- Challenge problem ---------------- *)

datatype typ = Anything
             | UnitT
             | IntT
             | TupleT of typ list
             | Datatype of string

                               
(* Exception to be raised if the types are invalid. *)
exception TypeError

              
(* Returns SOME t where t is the common type for the types of given patterns,
 * NONE if there are either no common type or incorrect type or unequal
 * number for a tuple. *)
fun typecheck_patterns (cst, ps) =
    let
        fun common_type (t1, Anything) = t1
          | common_type (Anything, t2) = t2
          | common_type (UnitT, UnitT) = UnitT
          | common_type (IntT, IntT) = IntT
          | common_type (TupleT t1, TupleT t2) =
            TupleT (map common_type (ListPair.zipEq (t1, t2)))
          | common_type (Datatype d1, Datatype d2) = 
            if d1 = d2 then Datatype d1 else raise TypeError
          | common_type _ = raise TypeError

        fun common_type_for_types [] = Anything
          | common_type_for_types [t] = t
          | common_type_for_types (t1::t2::ts) =
            common_type_for_types ((common_type (t1, t2))::ts)

        fun datatype_check (cs1, pt) =
            let
                fun helper (cs2, _, ct) =
                    cs1 = cs2
                    andalso (common_type (pt, ct); true)
                    handle TypeError => false
            in
                case List.find helper cst of
                    NONE => raise TypeError
                  | SOME (_, dt, _) => Datatype dt
            end
                
        fun pattern_type UnitP = UnitT
          | pattern_type (ConstP _) = IntT
          | pattern_type (TupleP tps) = TupleT (map pattern_type tps)
          | pattern_type (ConstructorP (cs1, cp)) =
            datatype_check (cs1, pattern_type cp)
          | pattern_type _ = Anything
    in
        SOME (common_type_for_types (map pattern_type ps))
        handle TypeError => NONE
             | UnequalLenghts => NONE 
    end
                                  

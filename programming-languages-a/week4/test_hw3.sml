(* Programming Assignment: Homework 3 tests *)

use "hw3.sml";


val test1 = only_capitals [] = []
val test2 = only_capitals ["h", "a", "A", "B", "c"] = ["A", "B"]
val test3 = only_capitals ["hello", "World", "the", "Earth"] = ["World", "Earth"]
val test4 = only_capitals ["hello", "world", "the"] = []

val test5 = longest_string1 [] = ""
val test6 = longest_string1 ["a", "ab", "c", "abc"] = "abc"
val test7 = longest_string1 ["one", "a", "b", "hi", "two"] = "one"
val test8 = longest_string1 ["a", "b", "one", "hi", "two"] = "one"

val test9 = longest_string2 [] = ""
val test10 = longest_string2 ["a", "ab", "c", "abc"] = "abc"
val test11 = longest_string2 ["one", "a", "b", "hi", "two"] = "two"
val test12 = longest_string2 ["a", "b", "one", "hi", "two"] = "two"

val test13 = longest_string3 [] = ""
val test14 = longest_string3 ["a", "ab", "c", "abc"] = "abc"
val test15 = longest_string3 ["one", "a", "b", "hi", "two"] = "one"
val test16 = longest_string3 ["a", "b", "one", "hi", "two"] = "one"

val test17 = longest_string4 [] = ""
val test18 = longest_string4 ["a", "ab", "c", "abc"] = "abc"
val test19 = longest_string4 ["one", "a", "b", "hi", "two"] = "two"
val test20 = longest_string4 ["a", "b", "one", "hi", "two"] = "two"

val test21 = longest_capitalized [] = ""
val test22 = longest_capitalized ["abc", "hello", "world"] = ""
val test23 = longest_capitalized ["abc", "hello", "A"] = "A"
val test24 = longest_capitalized ["Test", "no", "Yes", "NOPE"] = "Test"
val test25 = longest_capitalized ["longest", "But", "huh"] = "But"

val test26 = rev_string "" = ""
val test27 = rev_string "hello" = "olleh"
val test28 = rev_string "a" = "a"
val test29 = rev_string "ab" = "ba"

val test30 = count_wildcards Wildcard = 1
val test31 = count_wildcards (Variable "a") = 0
val test32 = count_wildcards UnitP = 0
val test33 = count_wildcards (ConstP 1) = 0
val test34 = count_wildcards (TupleP []) = 0
val test35 = count_wildcards (TupleP [UnitP, Wildcard, (ConstP 1)]) = 1
val test36 = count_wildcards (TupleP [Wildcard, UnitP, (TupleP [Wildcard, UnitP])]) = 2
val test37 = count_wildcards (ConstructorP ("a", (TupleP [UnitP, Wildcard, (ConstP 1)]))) = 1

val test38 = count_wild_and_variable_lengths Wildcard = 1
val test39 = count_wild_and_variable_lengths (Variable "a") = 1
val test40 = count_wild_and_variable_lengths (Variable "abc") = 3
val test41 = count_wild_and_variable_lengths UnitP = 0
val test42 = count_wild_and_variable_lengths (ConstP 1) = 0
val test43 = count_wild_and_variable_lengths (TupleP []) = 0
val test44 = count_wild_and_variable_lengths
                 (TupleP [Wildcard, (ConstP 1), (Variable "hi")]) = 3
val test45 = count_wild_and_variable_lengths
                 (TupleP [Wildcard, (TupleP [(Variable "a"), (Variable "b")]), Wildcard]) = 4
val test46 = count_wild_and_variable_lengths
                 (ConstructorP ("a", (TupleP [(Variable "hello"), Wildcard, UnitP]))) = 6

val test47 = count_some_var ("a", Wildcard) = 0
val test48 = count_some_var ("a", (Variable "b")) = 0
val test49 = count_some_var ("a", (Variable "a")) = 1
val test50 = count_some_var ("x", UnitP) = 0
val test51 = count_some_var ("x", (ConstP 1)) = 0
val test52 = count_some_var ("x", TupleP []) = 0
val test53 = count_some_var ("x", TupleP [(Variable "a"), (Variable "x"), (Variable "x")]) = 2
val test54 = count_some_var ("x", ConstructorP ("x", TupleP [Wildcard, (Variable "x")])) = 1

val test55 = check_pat Wildcard = true
val test56 = check_pat (Variable "b") = true
val test57 = check_pat UnitP = true
val test58 = check_pat (ConstP 1) = true
val test59 = check_pat (TupleP [(Variable "a"), (Variable "a")]) = false
val test60 = check_pat (TupleP [(Variable "a"), (Variable "b"), (Variable "c")]) = true
val test61 = check_pat
                 (TupleP [(Variable "a"), (TupleP [(Variable "b"), UnitP]), (Variable "b")])
             = false
val test62 = check_pat
                 (ConstructorP ("a", TupleP [(Variable "b"), TupleP [(Variable "c")]]))
             = true
val test63 = check_pat (TupleP []) = true
val test64 = check_pat (TupleP [(Variable "a")]) = true
val test65 = check_pat (ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])) = false
val test66 = check_pat (TupleP[ConstP 4,Wildcard,Variable "ba",TupleP[Variable "ab"]]) = true

val test67 = match (Const 5, Wildcard) = SOME []
val test68 = match (Unit, Wildcard) = SOME []
val test69 = match (Tuple [Const 5, Const 1, Unit], Wildcard) = SOME []
val test70 = match (Constructor ("SOME", Const 1), Wildcard) = SOME []
val test71 = match (Const 5, Variable "x") = SOME [("x",Const 5)]
val test72 = match (Unit, Variable "x") = SOME [("x", Unit)]
val test73 = match (Tuple [Const 5, Unit], Variable "x")
             = SOME [("x", Tuple [Const 5, Unit])]
val test74 = match (Constructor ("SOME", Unit), Variable "x")
             = SOME [("x", Constructor ("SOME", Unit))]
val test75 = match (Unit, UnitP) = SOME []
val test76 = match (Const 4, ConstP 4) = SOME []
val test77 = match (Const 4, ConstP 5) = NONE
val test78 = match (Tuple [Const 4, Unit], TupleP [ConstP 4, UnitP]) = SOME []
val test79 = match (Tuple [Const 4, Unit], TupleP [UnitP, ConstP 4]) = NONE
val test80 = match (Tuple [Const 4, Unit, Tuple []], TupleP [Variable "x", UnitP, Variable "y"])
             = SOME [("x", Const 4), ("y", Tuple [])]
val test81 = match (Tuple [Const 4], TupleP []) = NONE
val test82 = match (Constructor ("A", Unit), ConstructorP ("A", UnitP)) = SOME []
val test83 = match (Constructor ("A", Const 2), ConstructorP ("A", Wildcard)) = SOME []
val test84 = match (Constructor ("A", Const 2), ConstructorP ("B", Wildcard)) = NONE
val test85 = match (Constructor ("A", Const 2), ConstructorP ("A", UnitP)) = NONE
val test86 =
    match (Constructor ("A", Tuple [Const 4, Unit, Const 3]),
           ConstructorP ("A", TupleP [Variable "a", UnitP, Wildcard]))
    = SOME [("a", Const 4)]

val test87 = first_match (Const 3) [UnitP, Wildcard, (Variable "a")] = SOME []
val test88 = first_match (Const 3) [UnitP, (Variable "a"), ConstP 3] = SOME [("a", Const 3)]
val test89 = first_match (Const 3) [UnitP, TupleP [], ConstP 4] = NONE


(* ----------- Challenge problems tests -------------- *)
                                                                      
val test90 = typecheck_patterns ([], [ConstP 10, Variable "a"]) = SOME IntT
                                                                       
val test91 = typecheck_patterns
                 ([("SOME", "option", Anything),
                   ("NONE", "option", UnitT)],
                  [ConstP 10,
                   Variable "a",
                   ConstructorP("SOME", Variable "x")])
             = NONE
                   
val test92 = typecheck_patterns
                 ([],
                  [TupleP [Variable "a", ConstP 10, Wildcard],
                   TupleP [Variable "b", Wildcard, ConstP 11], Wildcard])
             = SOME (TupleT [Anything, IntT, IntT])
                    
val test93 = typecheck_patterns
                 ([("Red", "color", UnitT),
                   ("Green", "color", UnitT),
                   ("Blue", "color", UnitT)],
                  [ConstructorP("Red", UnitP),
                   Wildcard])
             = SOME (Datatype "color")
                    
val test94 = typecheck_patterns
                 ([("Sedan", "auto", Datatype "color"),
                   ("Truck", "auto", TupleT [IntT, Datatype "color"]),
                   ("SUV", "auto", UnitT)],
                  [ConstructorP ("Sedan", Variable "a"),
                   ConstructorP ("Truck", TupleP [Variable "b", Wildcard]),
                   Wildcard])
             = SOME (Datatype "auto")
                    
val test95 = typecheck_patterns
                 ([("Empty", "list", UnitT),
                   ("List", "list", TupleT [Anything, Datatype "list"])],
                  [ConstructorP ("Empty", UnitP),
                   ConstructorP ("List", TupleP [ConstP 10, ConstructorP ("Empty", UnitP)]),
                   Wildcard])
             = SOME (Datatype "list")
                    
val test96 = typecheck_patterns
                 ([("Empty", "list", UnitT),
                   ("List", "list", TupleT [Anything, Datatype "list"])],
                  [ConstructorP ("Empty", UnitP),
                   ConstructorP ("List", TupleP [Variable "k", Wildcard])])
             = SOME (Datatype "list")

val test97 = typecheck_patterns
                 ([("Empty", "list", UnitT),
                   ("List", "list", TupleT [Anything, Datatype "list"]),
                   ("Sedan", "auto", Datatype "color")],
                  [ConstructorP ("Empty", UnitP),
                   ConstructorP ("List", TupleP [ConstructorP ("Sedan", Variable "c"),
                                                 Wildcard])])
             = SOME (Datatype "list")
                    
val test98 = typecheck_patterns
                 ([],
                  [TupleP [Variable "x", Variable "y"],
                   TupleP [Wildcard, Wildcard]])
             = SOME (TupleT [Anything, Anything])

val test99 = typecheck_patterns
                 ([],
                  [TupleP [Wildcard, Wildcard],
                   TupleP[Wildcard, TupleP[Wildcard,Wildcard]]])
             = SOME (TupleT[Anything, TupleT[Anything, Anything]])
                            

(* Programming Assignment: Homework 2 tests *)

use "hw2.sml";


val test1 = all_except_option ("a", []) = NONE
val test2 = all_except_option ("a", ["b"]) = NONE
val test3 = all_except_option ("a", ["a", "b"]) = SOME ["b"]
val test4 = all_except_option ("a", ["b", "a", "c"]) = SOME ["b", "c"]
val test5 = all_except_option ("a", ["b", "c", "a"]) = SOME ["b", "c"]

val test6 = get_substitutions1 ([], "a") = []
val test7 = get_substitutions1 ([["a", "b"], ["c", "d"], ["e", "f"]], "g") = []
val test8 = get_substitutions1 ([["a", "b"], ["c", "d"], ["e", "a", "f"]], "a") = ["b", "e", "f"]
val test9 = get_substitutions1 ([["a", "b"], ["c"], ["a", "b", "c"]], "a") = ["b", "b", "c"]

val test10 = get_substitutions2 ([], "a") = []
val test11 = get_substitutions2 ([["a", "b"], ["c", "d"], ["e", "f"]], "g") = []
val test12 = get_substitutions2 ([["a", "b"], ["c", "d"], ["e", "a", "f"]], "a") = ["b", "e", "f"]
val test13 = get_substitutions2 ([["a", "b"], ["c"], ["a", "b", "c"]], "a") = ["b", "b", "c"]

val test14 = similar_names (
        [["a", "b"], ["c", "d"], ["e", "a", "f"]], {first="a", middle="m", last="l"}
    ) = [{first="a", middle="m", last="l"},
         {first="b", middle="m", last="l"},
         {first="e", middle="m", last="l"},
         {first="f", middle="m", last="l"}]
val test15 = similar_names (
        [["a", "b"], ["c"], ["a", "b", "c"]], {first="a", middle="m", last="l"}
    ) = [{first="a", middle="m", last="l"},
         {first="b", middle="m", last="l"},
         {first="b", middle="m", last="l"},
         {first="c", middle="m", last="l"}]

val test16 = card_color (Clubs, Ace) = Black
val test17 = card_color (Diamonds, Num 3) = Red
val test18 = card_color (Hearts, Queen) = Red
val test19 = card_color (Spades, Num 9) = Black

val test20 = card_value (Clubs, Ace) = 11
val test21 = card_value (Hearts, Queen) = 10
val test22 = card_value (Spades, Num 3) = 3
val test23 = card_value (Diamonds, King) = 10

val test24 = (remove_card ([], (Clubs, Ace), IllegalMove); false) handle IllegalMove => true
val test25 = (remove_card (
                   [(Clubs, Ace), (Hearts, Num 2), (Spades, King)],
                   (Diamonds, Ace),
                   IllegalMove
               ); false) handle IllegalMove => true
val test26 = remove_card (
        [(Clubs, Ace), (Hearts, Num 2), (Spades, King)], (Hearts, Num 2), IllegalMove
    ) = [(Clubs, Ace), (Spades, King)]
val test27 = remove_card (
        [(Clubs, Ace), (Hearts, Num 2), (Spades, King), (Hearts, Num 2)],
        (Hearts, Num 2),
        IllegalMove
    ) = [(Clubs, Ace), (Spades, King), (Hearts, Num 2)]

val test28 = all_same_color [] = true
val test29 = all_same_color [(Clubs, Ace)] = true
val test30 = all_same_color [(Clubs, Ace), (Hearts, Num 2)] = false
val test31 = all_same_color [(Clubs, Ace), (Spades, King)] = true
val test32 = all_same_color [(Diamonds, Ace), (Hearts, Queen)] = true

val test33 = sum_cards [] = 0
val test34 = sum_cards [(Clubs, Ace)] = 11
val test35 = sum_cards [(Clubs, Ace), (Hearts, Num 2), (Spades, King)] = 23

val test36 = score ([], 10) = 5
val test37 = score ([(Clubs, Ace)], 10) = 1
val test38 = score ([(Clubs, Ace), (Hearts, Num 2)], 10) = 9
val test39 = score ([(Clubs, Num 2), (Spades, Num 2)], 10) = 3

val test40 = officiate ([], [], 10) = 5
val test41 = officiate ([(Clubs,Ace)], [], 10) = 5
val test42 = officiate ([],[Draw], 10) = 5
val test43 = ((officiate ([], [Discard (Diamonds, Num 9)], 10);
               false) handle IllegalMove => true)
val test44 = officiate ([(Clubs, Ace)], [Draw, Discard (Clubs, Ace)], 2) = 13
val test45 = officiate ([(Hearts, Num 2), (Clubs, Num 4)], [Draw], 15) = 6
val test46 = officiate ([(Clubs, Ace), (Spades, Ace), (Clubs, Ace), (Spades, Ace)],
                        [Draw, Draw, Draw, Draw, Draw],
                        42)
             = 3
val test47 = ((officiate([(Clubs, Jack), (Spades, Num 8)],
                         [Draw, Discard (Hearts, Jack)],
                         42);
               false)
              handle IllegalMove => true)

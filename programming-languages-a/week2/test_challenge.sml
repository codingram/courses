(* Programming Assignment: Challenge problem tests *)

use "challenge.sml";


val test1 = remove_duplicates ([]) = []
val test2 = remove_duplicates ([1, 2, 3]) = [1, 2, 3]
val test3 = remove_duplicates ([1, 2, 2, 3, 3, 3, 3, 4, 4, 4]) = [1, 2, 3, 4]
val test4 = remove_duplicates ([1, 2, 3, 2, 3, 4, 5, 3, 1, 1, 4, 5, 4, 1, 2]) = [3, 5, 4, 1, 2]


val test5 = number_in_months_challenge (
        [(2020, 1, 1), (2020, 2, 3), (2020, 1, 3)], [1, 1, 2]
    ) = 3
            
val test6 = dates_in_months_challenge (
        [(2020, 1, 1), (2020, 2, 3), (2020, 1, 3)], [1, 1, 2]
    ) = [(2020, 1, 1), (2020, 1, 3), (2020, 2, 3)]

val test7 = get_nth_number ([1, 2, 3, 4], 4) = 4
val test8 = get_nth_number ([1, 2, 3, 4], 1) = 1

val test9 = reasonable_date (1900, 2, 29) = false
val test10 = reasonable_date (2000, 2, 29) = true
val test11 = reasonable_date (2100, 2, 29) = false
val test12 = reasonable_date (2019, 13, 20) = false
val test13 = reasonable_date (2019, 4, 31) = false
val test14 = reasonable_date (~2020, 1, 1) = false
val test15 = reasonable_date (2020, 2, 29) = true

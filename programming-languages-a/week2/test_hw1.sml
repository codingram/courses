(* Programming Assignment: Homework 1 tests *)

use "hw1.sml";

val test1 = is_older ((1, 2, 3), (2, 3, 4)) = true
val test2 = is_older ((2, 1, 3), (1, 2, 3)) = false
val test3 = is_older ((1, 2, 3), (1, 3, 3)) = true
val test4 = is_older ((1, 3, 5), (1, 2, 3)) = false
val test5 = is_older ((1, 3, 5), (1, 3, 6)) = true
val test6 = is_older ((1, 3, 5), (1, 3, 4)) = false
val test7 = is_older ((1, 2, 3), (1, 2, 3)) = false
val test8 = is_older ((1, 3, 4), (2, 3, 4)) = true

val test9 = number_in_month([], 1) = 0
val test10 = number_in_month([(2012, 2, 28), (2013, 12, 1)], 2) = 1
val test11 = number_in_month([(2012, 1, 28), (2013, 11, 1)], 3) = 0
val test12 = number_in_month([(2020, 1, 13), (2019, 3, 14), (2018, 1, 15)], 1) = 2

val test13 = number_in_months([], [1, 2]) = 0
val test14 = number_in_months([(2020, 2, 14), (2020, 3, 14)], []) = 0
val test15 = number_in_months([(2020, 2, 14), (2020, 4, 14), (2020, 3, 13)], [2, 3]) = 2
val test16 = number_in_months([(2020, 2, 14), (2020, 4, 14), (2020, 3, 13)], [1, 5]) = 0
val test17 = number_in_months([(2020, 1, 1), (2020, 1, 5), (2020, 5, 1), (2020, 5, 2)], [1, 5]) = 4

val test18 = dates_in_month([], 1) = []
val test19 = dates_in_month([(2012, 2, 28), (2013, 12, 1)], 2) = [(2012, 2, 28)]
val test20 = dates_in_month([(2012, 1, 28), (2013, 11, 1)], 3) = []
val test21 = dates_in_month([(2020, 1, 13), (2019, 3, 14), (2018, 1, 15)], 1) = [(2020, 1, 13), (2018, 1, 15)]

val test22 = dates_in_months([], [1, 2]) = []
val test23 = dates_in_months([(2020, 2, 14), (2020, 3, 14)], []) = []
val test24 = dates_in_months([(2020, 2, 14), (2020, 4, 14), (2020, 3, 13)], [2, 3]) = [(2020, 2, 14), (2020, 3, 13)]
val test25 = dates_in_months([(2020, 2, 14), (2020, 4, 14), (2020, 3, 13)], [1, 5]) = []
val test26 = dates_in_months([(2020, 1, 1), (2020, 1, 5), (2020, 5, 1), (2020, 5, 2)], [1, 5]) = [(2020, 1, 1), (2020, 1, 5), (2020, 5, 1), (2020, 5, 2)]

val test27 = get_nth(["this", "is", "a", "test"], 1) = "this"
val test28 = get_nth(["this", "is", "a", "test"], 2) = "is"
val test29 = get_nth(["this", "is", "a", "test"], 3) = "a"
val test30 = get_nth(["this", "is", "a", "test"], 4) = "test"

val test31 = date_to_string(2020, 1, 5) = "January 5, 2020"
val test32 = date_to_string(2016, 11, 24) = "November 24, 2016"

val test33 = number_before_reaching_sum(1, [1]) = 0
val test34 = number_before_reaching_sum(1, [2]) = 0
val test35 = number_before_reaching_sum(3, [2, 4]) = 1
val test36 = number_before_reaching_sum(6, [1, 1, 1, 1, 1, 1]) = 5

val test37 = what_month 1 = 1
val test38 = what_month 31 = 1
val test39 = what_month 32 = 2
val test40 = what_month 59 = 2
val test41 = what_month 60 = 3
val test42 = what_month 90 = 3
val test43 = what_month 91 = 4
val test44 = what_month 120 = 4
val test45 = what_month 121 = 5
val test46 = what_month 151 = 5
val test47 = what_month 152 = 6
val test48 = what_month 181 = 6
val test49 = what_month 182 = 7
val test50 = what_month 212 = 7
val test51 = what_month 213 = 8
val test52 = what_month 243 = 8
val test53 = what_month 244 = 9
val test54 = what_month 273 = 9
val test55 = what_month 274 = 10
val test56 = what_month 304 = 10
val test57 = what_month 305 = 11
val test58 = what_month 334 = 11
val test59 = what_month 335 = 12
val test60 = what_month 365 = 12

val test61 = month_range(35, 28) = []
val test62 = month_range(28, 28) = [1]
val test63 = month_range(28, 35) = [1, 1, 1, 1, 2, 2, 2, 2]

val test64 = oldest([]) = NONE
val test65 = oldest([(2020, 1, 1)]) = SOME (2020, 1, 1)
val test66 = oldest([(2020, 1, 1), (2019, 1, 1)]) = SOME (2019, 1, 1)
val test67 = oldest([(2018, 1, 1), (2020, 1, 1)]) = SOME (2018, 1, 1)
val test68 = oldest([(2018, 1, 1), (2015, 1, 1), (2014, 1, 1)]) = SOME (2014, 1, 1)


(* CSE 341 problem tests *)

val test69 = cumulative_sum [12, 27, 13] = [12, 39, 52]
val test70 = cumulative_sum [] = []
val test71 = cumulative_sum [12] = [12]
val test72 = cumulative_sum [12, 13] = [12, 25]


(* Challenge problem tests *)

val ctest1 = remove_duplicates ([]) = []
val ctest2 = remove_duplicates ([1, 2, 3]) = [1, 2, 3]
val ctest3 = remove_duplicates ([1, 2, 2, 3, 3, 3, 3, 4, 4, 4]) = [1, 2, 3, 4]
val ctest4 = remove_duplicates ([1, 2, 3, 2, 3, 4, 5, 3, 1, 1, 4, 5, 4, 1, 2]) = [3, 5, 4, 1, 2]


val ctest5 = number_in_months_challenge (
        [(2020, 1, 1), (2020, 2, 3), (2020, 1, 3)], [1, 1, 2]
    ) = 3

val ctest6 = dates_in_months_challenge (
        [(2020, 1, 1), (2020, 2, 3), (2020, 1, 3)], [1, 1, 2]
    ) = [(2020, 1, 1), (2020, 1, 3), (2020, 2, 3)]

val ctest7 = get_nth_number ([1, 2, 3, 4], 4) = 4
val ctest8 = get_nth_number ([1, 2, 3, 4], 1) = 1

val ctest9 = reasonable_date (1900, 2, 29) = false
val ctest10 = reasonable_date (2000, 2, 29) = true
val ctest11 = reasonable_date (2100, 2, 29) = false
val ctest12 = reasonable_date (2019, 13, 20) = false
val ctest13 = reasonable_date (2019, 4, 31) = false
val ctest14 = reasonable_date (~2020, 1, 1) = false
val ctest15 = reasonable_date (2020, 2, 29) = true

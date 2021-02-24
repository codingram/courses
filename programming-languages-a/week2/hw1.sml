(* Programming Assignment: Homework 1 *)

(* Date is (int * int * int) -> (Year, Month, Day) *)

(* Returns true if date1 is older than date2, false otherwise *)
fun is_older (date1: int * int * int, date2: int * int * int) =
    if #1 date1 = #1 date2
    then
        if #2 date1 = #2 date2
        then #3 date1 < #3 date2
        else #2 date1 < #2 date2
    else #1 date1 < #1 date2


(* Return how many dates in the list are in the given month *)
fun number_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then 0
    else
        if #2 (hd dates) = month
        then 1 + number_in_month(tl dates, month)
        else number_in_month(tl dates, month)


(* Return the number of dates in the list of dates that are in
any of the months in the list of months. *)
fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)


(* Returns a list holding the dates from the argument list of dates that are
in the month *)
fun dates_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then []
    else
        if #2 (hd dates) = month
        then hd dates :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)


(* Returns a list holding the dates from the argument list of dates
that are in any of the months in the list of months. *)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


(* Returns the nth element of the list where the head of the list is 1st. *)
fun get_nth (strs: string list, nth: int) =
    if nth = 1
    then hd strs
    else get_nth(tl strs, nth - 1)


(* Returns the given date of the form January 20, 2020 *)
fun date_to_string (date: int * int * int) =
    let
        val months: string list = [
            "January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December"
        ]
    in
        get_nth(months, #2 date)
        ^ " "
        ^ Int.toString(#3 date)
        ^ ", "
        ^ Int.toString(#1 date)
    end


(* Returns an int such that the first n elements of the given list add to less
than given sum, but the first n + 1 elements of the list add to sum or more. *)
fun number_before_reaching_sum (sum: int, numbers: int list) =
    if hd numbers >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - hd numbers, tl numbers)


(* Returns what month the given day is in (1 for January, 2 for February, etc.) *)
fun what_month (day: int) =
    let
        val ndays_in_month: int list = [
            31,
            28,
            31,
            30,
            31,
            30,
            31,
            31,
            30,
            31,
            30,
            31
        ]
    in
        number_before_reaching_sum(day, ndays_in_month) + 1
    end


(* Returns an int list [m1,m2,...,mn] where m1 is the month of day1,
m2 is the month of day1+1, ..., and mn is the month of day day2. *)
fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month day1 :: month_range(day1 + 1, day2)


(* Returns the oldest date in the given list, NONE if empty *)
fun oldest (dates: (int * int * int) list) =
    if null dates
    then NONE
    else
        let
            val tl_oldest = oldest(tl dates)
        in
            if isSome tl_oldest andalso is_older(valOf tl_oldest, hd dates)
            then tl_oldest
            else SOME (hd dates)
        end


(* Challenge Problems *)

(* Remove all the duplicate numbers from the given list and return the new list *)
(* NOTE: The order is not maintained. *)
fun remove_duplicates (numbers: int list) =
    if null numbers
    then []
    else
        let
            (* Check whether the element is present in the elementlist *)
            fun contains (element: int, elementlist: int list) =
                not (null elementlist)
                andalso (element = hd elementlist
                         orelse contains(element, tl elementlist))

            val tl_remove_dups = remove_duplicates(tl numbers)
        in
            if contains(hd numbers, tl_remove_dups)
            then tl_remove_dups
            else hd numbers :: tl_remove_dups
        end


(* Alternate version of 'number_in_months' function where the function can handle
 duplicate month values in the months list. *)
fun number_in_months_challenge (dates: (int * int * int) list, months: int list) =
    number_in_months (dates, remove_duplicates months)


(* Alternate version of 'dates_in_months' function where the function can handle
 duplicate month values in the months list. *)
fun dates_in_months_challenge (dates: (int * int * int) list, months: int list) =
    dates_in_months (dates, remove_duplicates months)


fun get_nth_number (numbers: int list, nth: int) =
    if nth = 1
    then hd numbers
    else get_nth_number(tl numbers, nth - 1)


(* Check whether the given date is reasonable or not. *)
(* Algorithm steps:
 - Check whether a year is positive or not
 - Check whether a month is between 1 and 12 inclusive
 - If the month is 2 (February) and if the given year is a leap year, check whether the
   date is <= 29
 - If the month is not 2 or if the given year is not a leap year, check whether the
   date is <= the number of days for the respective month from 'ndays_in_months'. *)
fun reasonable_date (date: int * int * int) =
    let
        val year: int = #1 date
        val month: int = #2 date
        val day: int = #3 date

        (* Number of days in months for a non-leap year *)
        val ndays_in_months: int list = [
            31,
            28,
            31,
            30,
            31,
            30,
            31,
            31,
            30,
            31,
            30,
            31
        ]

        (* Check whether the given year is a leap year or not. *)
        (* Leap years are years that are either:
         - Divisible by 400
         - Divisible by 4 but not divisible by 100 *)
        fun is_leap_year (year: int) =
            year mod 400 = 0
            orelse year mod 4 = 0
                   andalso year mod 100 <> 0
    in
        year > 0
        andalso 1 <= month
        andalso month <= 12
        (* By checking whether the month is 2 or not, we don't have to perform the
         leap year check for every date. *)
        andalso (month = 2
                 andalso is_leap_year year
                 andalso day <= 29
                 orelse day <= get_nth_number (ndays_in_months, month))
    end


(* Extra problems from CSE 341, Spring 2019 Assignment 1
 *
 * This includes only the problems which differ from the Coursera Assignment *)


(* Returns a list of the partial sums of the given list of numbers *)
fun cumulative_sum (numbers: int list) =
    let
        fun loop (numbers: int list, acc: int) =
            if null numbers
            then []
            else
                let val sum = hd numbers + acc
                in sum::(loop (tl numbers, sum))
                end
    in
        loop (numbers, 0)
    end

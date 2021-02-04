(* Programming Assignment: Homework 1 *)

(* Date is (int * int * int) -> (Year, Month, Day) *)

(* Returns true if date1 is older than date2, false otherwise *)
fun is_older (date1: int * int * int, date2: int * int * int) =
    if (#1 date1) = (#1 date2)
    then
        if (#2 date1) = (#2 date2)
        then (#3 date1) < (#3 date2)
        else (#2 date1) < (#2 date2)
    else (#1 date1) < (#1 date2)


(* Return how many dates in the list are in the given month *)
fun number_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then 0
    else
        if (#2 (hd dates)) = month
        then 1 + number_in_month((tl dates), month)
        else number_in_month((tl dates), month)


(* Return the number of dates in the list of dates that are in
any of the months in the list of months. *)
fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))


(* Returns a list holding the dates from the argument list of dates that are
in the month *)
fun dates_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then []
    else
        if (#2 (hd dates)) = month
        then (hd dates) :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)


(* Returns a list holding the dates from the argument list of dates
that are in any of the months in the list of months. *)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))


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
        get_nth(months, (#2 date))
        ^ " "
        ^ Int.toString(#3 date)
        ^ ", "
        ^ Int.toString(#1 date)
    end


(* Returns an int such that the first n elements of the given list add to less
than given sum, but the first n + 1 elements of the list add to sum or more. *)
fun number_before_reaching_sum (sum: int, numbers: int list) =
    if (hd numbers) >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)


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
    else (what_month day1) :: month_range(day1 + 1, day2)


(* Returns the oldest date in the given list, NONE if empty *)
fun oldest (dates: (int * int * int) list) =
    if null dates
    then NONE
    else
        let
            val tl_oldest = oldest(tl dates)
        in
            if (isSome tl_oldest) andalso is_older(valOf tl_oldest, hd dates)
            then tl_oldest
            else SOME (hd dates)
        end

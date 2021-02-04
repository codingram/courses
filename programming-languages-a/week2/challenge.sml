(* Challenge Problems *)

use "hw1.sml";

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
                andalso (element = (hd elementlist)
                         orelse contains(element, (tl elementlist)))

            val tl_remove_dups = remove_duplicates(tl numbers)
        in
            if contains(hd numbers, tl_remove_dups)
            then tl_remove_dups
            else (hd numbers) :: tl_remove_dups
        end
            

(* Alternate version of 'number_in_months' function where the function can handle
 duplicate month values in the months list. *)
fun number_in_months_challenge (dates: (int * int * int) list, months: int list) =
    number_in_months (dates, remove_duplicates months)

                     
(* Alternate version of 'dates_in_months' function where the function can handle
 duplicate month values in the months list. *)                         
fun dates_in_months_challenge (dates: (int * int * int) list, months: int list) =
    dates_in_months (dates, remove_duplicates months)


fun get_nth (numbers: int list, nth: int) =
    if nth = 1
    then hd numbers
    else get_nth(tl numbers, nth - 1)
                

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
            ((year mod 400) = 0)
            orelse (((year mod 4) = 0)
                    andalso ((year mod 100) <> 0))
    in
        year > 0
        andalso 1 <= month
        andalso month <= 12
        (* By checking whether the month is 2 or not, we don't have to perform the
         leap year check for every date. *)
        andalso (((month = 2)
                  andalso (is_leap_year year)
                  andalso (day <= 29))
                 orelse (day <= (get_nth (ndays_in_months, month))))
    end

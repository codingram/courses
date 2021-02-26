(* CSE 341, HW2 Provided Code *)

(* Main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* Some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]


(* Some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare

(* Convert an int to a real *)
val int_to_real = Real.fromInt

(* Absolute value of a real *)
val real_abs = Real.abs

(* Convert a real to a string *)
val real_to_string = Real.toString

(* Return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit


(* We now load 3 files with police data represented as values of type json.
   Each file binds one variable: small_incident_reports (10 reports),
   medium_incident_reports (100 reports), and large_incident_reports
   (1000 reports) respectively.

   However, the large file is commented out for now because it will take
   about 15 seconds to load, which is too long while you are debugging
   earlier problems.  In string format, we have ~10000 records -- if you
   do the challenge problem, you will be able to read in all 10000 quickly --
   it's the "trick" of giving you large SML values that is slow.
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";
use "parsed_medium_police.sml";


(* uncomment when you are ready to do the problems needing the large report*)
use "parsed_large_police.sml";

val large_incident_reports_list =
    case large_incident_reports of
        Array js => js
      | _ => raise (Fail "expected large_incident_reports to be an array")



(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;



(**** PUT PROBLEMS 1-8 HERE ****)

(* Problem 1: Return a JSON array of JSON objects where each object holds two
 * fields 'n' and 'b' *)
fun make_silly_json i =
    let
        fun loop 0 = []
          | loop i = Object [("n", Num (int_to_real i)), ("b", True)]::(loop (i - 1))
    in
        Array (loop i)
    end


(* Problem 2: Return SOME v1 if (k1,v1) is the pair in the list closest to the
 * beginning * of the list for which k and k1 are equal, otherwise return NONE. *)
fun assoc (k, xs) =
    case xs of
         [] => NONE
       | (k1, v1)::rest => if k = k1 then SOME v1 else assoc (k, rest)


(* Problem 3: Return SOME v if the given object has a field named f where v is
 * the content of the field, otherwise return NONE. *)
fun dot (Object jo, f) = assoc (f, jo)
  | dot (_, _) = NONE


(* Problem 4: Returns a list holding all of the given JSON Object field names. *)
fun one_fields (Object json_obj) =
    let
        fun loop ([], acc) = acc
          | loop ((field, value)::rest, acc) =
          loop (rest, field::acc)
    in
        loop (json_obj, [])
    end
  | one_fields (_) = []


(* Problem 5: Return true if and only if no string appears more than once in the
 * given string list. *)
fun no_repeats xs =
    length xs = length (dedup xs)


(* Problem 6: Return true if and only if no JSON Object (arbitrarily nested)
 * for the given JSON argument has repeated field names. *)
fun recursive_no_field_repeats j =
    let
        fun check_array [] = true
          | check_array (first::rest) = recursive_no_field_repeats first
                                        andalso check_array rest
        fun check_object [] = true
          | check_object ((_,value)::rest) = recursive_no_field_repeats value
                                             andalso check_object rest
    in
        case j of
            Array arr => check_array arr
          | Object obj => no_repeats (one_fields (Object obj))
                          andalso check_object obj
          | _ => true
    end


(* Problem 7: Return a list where each string from the given list is paired with
 * the number of times it occurs (The order of the output list does not matter.)
 * If the given list is not sorted in a consistent manner, the given exc will
 * be raised. *)
fun count_occurrences (xs, exc) =
    let
        fun loop ([], cs, cc, acc, _) = (cs, cc)::acc
          | loop (x::rest, cs, cc, acc, order) =
          case String.compare (cs, x) of
               EQUAL => loop (rest, cs, cc + 1, acc, order)
             | comp => if order = comp orelse order = EQUAL
                       then loop (rest, x, 1, (cs, cc)::acc, comp)
                       else raise exc
    in
        case xs of [] => [] | x::rest => loop (rest, x, 1, [], EQUAL)
    end


(* Problem 8: Return a list of string which is the value of a JSON string which
 * itself is a value of a JSON object field where field matches the given string.
 * Assumption: No single object has repeated field names. *)
fun string_values_for_field (_, []) = []
  | string_values_for_field (s, j::rest) =
  case dot (j, s) of
       SOME (String rs) => rs::string_values_for_field (s, rest)
     | _ => string_values_for_field (s, rest)




(* histogram and histogram_for_field are provided, but they use your
   count_occurrences and string_values_for_field, so uncomment them
   after doing earlier problems *)

(* histogram_for_field takes a field name f and a list of objects js and
   returns counts for how often a string is the contents of f in js. *)

exception SortIsBroken


fun histogram (xs: string list): (string * int) list =
    let
        fun compare_strings (s1: string, s2: string): bool = s1 > s2

        val sorted_xs = ListMergeSort.sort compare_strings xs
        val counts = count_occurrences (sorted_xs,SortIsBroken)

        fun compare_counts ((s1: string, n1: int), (s2: string, n2: int)): bool =
            n1 < n2 orelse (n1 = n2 andalso s1 < s2)
    in
        ListMergeSort.sort compare_counts counts
    end


fun histogram_for_field (f,js) =
  histogram (string_values_for_field (f, js))




(**** PUT PROBLEMS 9-11 HERE ****)

(* Problem 9: *)
fun filter_field_value (_, _, []) = []
  | filter_field_value (fs, s, j::rest) =
  case dot (j, fs) of
       SOME (String rs) => if s = rs
                           then j::(filter_field_value (fs, s, rest))
                           else filter_field_value (fs, s, rest)
     | _ => filter_field_value (fs, s, rest)


(* Problem 10: *)
val large_event_clearance_description_histogram =
    histogram_for_field ("event_clearance_description", large_incident_reports_list)


(* Problem 11: *)
val large_hundred_block_location_histogram =
    histogram_for_field ("hundred_block_location", large_incident_reports_list)


;Control.Print.printDepth := 3;
Control.Print.printLength := 3;


(**** PUT PROBLEMS 12-15 HERE ****)

(* Problem 12: *)
val forty_third_and_the_ave_reports =
    filter_field_value (
        "hundred_block_location",
        "43XX BLOCK OF UNIVERSITY WAY NE",
        large_incident_reports_list
    )


(* Problem 13: *)
val forty_third_and_the_ave_event_clearance_description_histogram =
    histogram_for_field ("event_clearance_description", forty_third_and_the_ave_reports)


(* Problem 14: *)
val nineteenth_and_forty_fifth_reports =
    filter_field_value (
        "hundred_block_location",
        "45XX BLOCK OF 19TH AVE NE",
        large_incident_reports_list
    )


(* Problem 15: *)
val nineteenth_and_forty_fifth_event_clearance_description_histogram =
    histogram_for_field ("event_clearance_description", nineteenth_and_forty_fifth_reports)


;Control.Print.printDepth := 20;
Control.Print.printLength := 20;


(**** PUT PROBLEMS 16-19 HERE ****)

(* Problem 16: *)
fun concat_with (_, []) = ""
  | concat_with (_, [s]) = s
  | concat_with (sep, [s1, s2]) = s1 ^ sep ^ s2
  | concat_with (sep, s1::rest) =
  s1 ^ sep ^ concat_with (sep, rest)


(* Problem 17: *)
fun quote_string str = "\"" ^ str ^ "\""


(* Problem 18: *)
fun real_to_string_for_json r =
    if Real.signBit r then "-" ^ Real.toString (Real.abs r) else Real.toString r


(* Problem 19: *)
fun json_to_string js =
    let
        fun array_to_string_list [] = []
          | array_to_string_list (j::rest) =
          json_to_string j::array_to_string_list rest

        fun object_to_string_list [] = []
          | object_to_string_list ((field,value)::rest) =
          quote_string field ^ ":" ^ json_to_string value::object_to_string_list rest
    in
        case js of
             Num n => real_to_string_for_json n
           | String s => quote_string s
           | False => "false"
           | True => "true"
           | Null => "null"
           | Array arr => "[" ^ concat_with (",", array_to_string_list arr) ^ "]"
           | Object obj => "{" ^ concat_with (",", object_to_string_list obj) ^ "}"
    end

(* Problem 19: Alternate version using features from the next week's lecture:
 * Anonymous functions, first class functions, standard library functions *)
(*
fun json_to_string (Num n) = real_to_string_for_json n
  | json_to_string (String s) = quote_string s
  | json_to_string False = "false"
  | json_to_string True = "true"
  | json_to_string Null = "null"
  | json_to_string (Array arr) = "[" ^ concat_with (", ", List.map json_to_string arr) ^ "]"
  | json_to_string (Object obj) =
  "{" ^ concat_with (", ",
        List.map (fn (f,v) => quote_string f ^ " : " ^ json_to_string v) obj) ^ "}"
 *)

(* For CHALLENGE PROBLEMS, see hw2challenge.sml *)

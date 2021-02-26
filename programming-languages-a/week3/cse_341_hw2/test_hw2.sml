(* CSE 341, Homework 2 Tests *)

use "hw2.sml";

(* You will surely want to add more! *)

(* WARNING: because real is not an eqtype, json is not an eqtype, so you cannot
   use = on anything including something of type json.
   See test1, test3, and test9 for examples of how to work around this. *)

val epsilon = 0.0001
fun check_real (r1,r2) = Real.abs (r1 - r2) < epsilon

val test1a =
    case make_silly_json 2 of
        Array [Object [("n",Num x),
                       ("b",True)],
               Object [("n",Num y),
                       ("b",True)]]
        => check_real (x,2.0) andalso check_real(y,1.0)
      | _ => false

val test1b =
    case make_silly_json 0 of
         Array [] => true
       | _ => false

val test1c =
    case make_silly_json 1 of
         Array [Object [("n", Num x), ("b", True)]] => check_real (x, 1.0)
       | _ => false


val test2a = assoc ("foo", [("bar",17),("foo",19)]) = SOME 19
val test2b = assoc ("a", [("b", 2), ("c", 1)]) = NONE

val test3a = case dot (Object [("a", False), ("ok", True)], "ok") of
                  SOME True => true
                |  _ => false
val test3b = case dot (Array [Object [("ok", True)]], "ok") of
                  NONE => true
                | _ => false
val test3c = case dot (Object [("a", True), ("b", Num 2.0)], "c") of
                  NONE => true
                | _ => false

val test4a = one_fields (Object [("foo", Num 2.0), ("bar", True), ("ok", False)]) =
    rev ["foo","bar","ok"]
val test4b = one_fields (Array [Object [("foo", True)], Num 2.0, True]) = []
val test4c = one_fields (Object []) = []

val test5a = not (no_repeats ["foo","bar","foo"])
val test5b = no_repeats ["a", "b", "c", "d", "e"]
val test5c = not (no_repeats ["a", "b", "a", "c", "b", "c", "a", "e", "f"])

val nest1 =
  Array
    [Object [],
     Object [("a",True),("b",Object [("foo",True),("foo",True)]),("c",True)],
     Object []] : json

val nest2 =
  Array
    [Object [],
     Object
       [("a",True),("b",Object [("foo",True),("bar",True)]),("c",Num 2.0),
        ("d",
         Array
           [Object [],
            Object
              [("a",True),("b",Object [("foo",True),("foo",True)]),("c",True)],
            Object []])]] : json

val nest3 =
  Array
    [Object [],
     Object
       [("a",True),("b",Object [("foo",True),("bar",True)]),("c",Num 2.0),
        ("d",Array [Num 2.0,Object [("a",True)]])]] : json

val test6a = not (recursive_no_field_repeats nest1)
val test6b = recursive_no_field_repeats (Num 2.0)
val test6c = not (recursive_no_field_repeats nest2)
val test6d = recursive_no_field_repeats nest3


(* Any order is okay, so it's okay to fail this test due to order *)
val test7a = count_occurrences (["a", "a", "b"], Fail "") = [("b",1),("a",2)]
val test7b = count_occurrences (["b", "a", "b"], Fail "") = []
             handle (Fail "") => true
val test7c = count_occurrences (["a", "a", "b", "c", "a"], Fail "") = []
             handle (Fail "") => true
val test7d = count_occurrences ([], Fail "") = []
val test7e = count_occurrences (["a"], Fail "") = [("a",1)]
val test7g = count_occurrences (["a", "a", "a", "b", "b", "c", "c", "d"], Fail "") =
             [("d",1),("c",2),("b",2),("a",3)]
val test7h = count_occurrences (["d", "d", "c", "b", "b", "a"], Fail "") =
             [("a",1),("b",2),("c",1),("d",2)]


val test8a = string_values_for_field ("x", [Object [("a", True),("x", String "foo")],
                                           Object [("x", String "bar"), ("b", True)]])
            = ["foo","bar"]
val test8b = string_values_for_field ("a", []) = []
val test8c = string_values_for_field ("a", [Array [String "t", String "t"],
                                            String "foo",
                                            Object [("a", Object [("a", String "no")])],
                                            Object [("a", String "b")],
                                            Object [("a", String "b")],
                                            Object [("a", String "c")]]) = ["b", "b", "c"]

val test9a =
   case filter_field_value ("x", "foo",
                            [Object [("x", String "foo"), ("y", String "bar")],
                             Object [("x", String "foo"), ("y", String "baz")],
                             Object [("x", String "a")],
                             Object []]) of
       [Object [("x",String "foo"),("y",String "bar")],
        Object [("x",String "foo"),("y",String "baz")]] => true
     | _ => false
val test9b = case filter_field_value ("x", "foo", []) of [] => true | _ => false

(*****)

val test16a = concat_with("a", ["b", "n", "na"]) = "banana"
val test16b = concat_with("-", []) = ""
val test16c = concat_with("-", ["this"]) = "this"
val test16d = concat_with("-", ["this", "is"]) = "this-is"
val test16e = concat_with("-", ["this", "is", "a"]) = "this-is-a"

val test17 = quote_string "foo" = "\"foo\""

val test18a = real_to_string_for_json 4.305 = "4.305"
val test18b = real_to_string_for_json ~4.305 = "-4.305"

val test19 = json_to_string json_obj =
             "{\"foo\" : 3.14159, \"bar\" : [1.0, \"world\", null], \"ok\" : true}";

(* End of tests for required problems. A few commented-out tests for
   challenge problems follow.  The tests below are in a different style
   where we use pattern-matching in val-bindings for the expected output. *)

use "challenge_hw2.sml";

(* Tests for consume_string_literal *)
val test20a = consume_string_literal (String.explode "\"foo\" : true") =
    ("foo",[#" ",#":",#" ",#"t",#"r",#"u",#"e"])
val test20b = (consume_string_literal (String.explode "hello"); false)
                handle Fail _ => true
val test20c = (consume_string_literal (String.explode "\"hello"); false)
                handle Fail _ => true



(* Tests for consume_keyword *)
val test21a = (FalseTok, [#" ",#"f",#"o",#"o"]) =
  consume_keyword (String.explode "false foo")
val test21b = (consume_keyword (String.explode "foofalse foo"); false)
                handle Fail _ => true


(* Tests consume_number *)
val test22a = ("1",[]) = consume_num (String.explode "1")
val test22b = ("~1.23e17",[]) = consume_num (String.explode "~1.23e17")

(* Tests for tokenize_char_list. You'll want more. *)
val test23a = [LBrace, StringLit "foo", Colon, NumLit "3.14", Comma,
     StringLit "bar", Colon, LBracket, TrueTok, Comma,
     FalseTok, RBracket, RBrace] =
  tokenize_char_list (String.explode "{ \"foo\" : 3.14, \"bar\" : [true, false] }")


(* Tests for parse_string *)
val test24a = ("foo", [FalseTok]) =
  parse_string ([StringLit "foo", FalseTok])
val test24b = (parse_string [FalseTok, TrueTok]; false) handle Fail _ => true


(* Tests for expect *)
val test25a = [FalseTok] = expect (Colon, [Colon, FalseTok])
val test25b = (expect (Colon, []); false) handle Fail _ => true


(* Tests for parse_json. You'll probably want way more. *)
val test26a = (Object [("foo", Null),("bar",Array [True,False])],[]) =
  parse_json (tokenize "{ \"foo\" : null, \"bar\" : [true, false] }")

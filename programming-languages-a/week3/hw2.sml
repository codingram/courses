(* Programming Assignment: Homework 2 *)

(* If the function body is made up of only one case expression and there are no other
 * steps after the case expression, I prefer multiple cases in function binding. *)

(* ----------- First name substitutions ----------- *)

(* Returns true if s1 and s2 are the same string, false otherwise *)
fun same_string (s1: string, s2: string) =
    s1 = s2


(* Return NONE if given string is not in the list, else return SOME list *)
fun all_except_option (s, slist) =
    case slist of
        [] => NONE
      | h::t => if same_string (h, s)
                then SOME t
                else case all_except_option (s, t) of
                         NONE => NONE
                       | SOME lst => SOME (h::lst)


(* Return a list of string which has all the strings that are in some list in
 * substitutios that also has s, but s itself is not in the result. *)
fun get_substitutions1 (subs, s) =
    case subs of
        [] => []
      | h::t => case all_except_option (s, h) of
                    NONE => [] @ get_substitutions1 (t, s)
                  | SOME lst => lst @ get_substitutions1 (t, s)


(* Tail recursive version of get_substitutions1 *)
fun get_substitutions2 (subs, s) =
    let
        fun helper (subs, acc) =
            case subs of
                [] => acc
              | h::t => case all_except_option (s, h) of
                            NONE => helper (t, acc)
                          | SOME lst => helper (t, acc @ lst)
    in
        helper (subs, [])
    end


(* Returns a list of full names which can be produced by substituting for the first
 * name using substitutions. The result will begin with the original name, then have
 * 0 or more other names. *)
fun similar_names (subs, full_name) =
    let
        val {first=f, middle=m, last=l} = full_name
        fun substitute name_list =
            case name_list of
                [] => []
              | h::t => {first=h, middle=m, last=l}::substitute t
    in
        full_name::substitute (get_substitutions2 (subs, f))
    end


(* ----------- Solitaire card game ----------- *)

(* You may assume that Num is always used with values 2, 3, ..., 10
 * though it will not really come up *)
datatype suit = Clubs
              | Diamonds
              | Hearts
              | Spades

datatype rank = Ace
              | King
              | Queen
              | Jack
              | Num of int

(* Type alias *)
type card = suit * rank

datatype color = Red
               | Black

datatype move = Discard of card
              | Draw

exception IllegalMove


(* Returns the color of the given card. *)
fun card_color card =
    case card of
        (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Spades, _) => Black


(* Returns the value of the given card. *)
fun card_value card =
    case card of
        (_, Ace) => 11
      | (_, Num n) => n
      | _ => 10


(* Removes the first occurence of the given card in the given card list.
 * Raises the given exception if the card is not present in the list. *)
fun remove_card (card_list, card, exc) =
    case card_list of
        [] => raise exc
      | h::t => if card = h
                then t
                else h::(remove_card (t, card, exc))


(* Returns true if all the given cards of the same color, false otherwise. *)
fun all_same_color cards =
    case cards of
        [] => true
      | _::[] => true
      | first::second::rest => card_color first = card_color second
                               andalso all_same_color (second::rest)


(* Returns the sum of values of all the given cards. *)
fun sum_cards cards =
    let
        fun helper (cards, acc) =
            case cards of
                [] => acc
              | h::t => helper (t, acc + card_value h)
    in
        helper (cards, 0)
    end


(* Returns the score of the given held cards. *)
fun score (cards, goal) =
    let
        val sum = sum_cards cards
        val pre = if sum > goal
                  then 3 * (sum - goal)
                  else goal - sum
    in
        if all_same_color cards
        then pre div 2
        else pre
    end


(* Runs the game and returns the final score of the game after processing
 * (some or all of) the moves in order. Raises the IllegalMove exception
 * for illegal moves. *)
fun officiate (cards, moves, goal) =
    let
        fun helper (cards, moves, held_cards) =
            case (moves, cards) of
                ([], _) => score (held_cards, goal)
              | ((Discard card)::remaining_moves, _) =>
                    helper (cards,
                            remaining_moves,
                            remove_card (held_cards, card, IllegalMove))
              | (Draw::remaining_moves, []) => score (held_cards, goal)
              | (Draw::remaining_moves, first::remaining_cards) =>
                    if sum_cards (first::held_cards) > goal
                    then score (first::held_cards, goal)
                    else helper (remaining_cards,
                                 remaining_moves,
                                 first::held_cards)
    in
        helper (cards, moves, [])
    end


(* ----------- Challenge Problems ----------- *)

(* Separate out the aces from the given card list and return a list of normal cards,
 * and a list of ace cards. *)
fun separate_aces cards =
    let
        fun loop (cards, normal, aces) =
            case cards of
                [] => (normal, aces)
              | (suit, Ace)::rest => loop (rest, normal, (suit, Ace)::aces)
              | c::rest => loop (rest, c::normal, aces)
    in
        loop (cards, [], [])
    end


(* The algorithm works as follows:
 * First, separate out the aces from the cards using the separate_aces function.
 * Set the lowest to the score when all the aces are 11 and start the loop.
 * Start setting aces to value 1 one by one, calculate the score and compare
 * it with the current lowest and replace the lowest as per the output.
 *
 * Eg: c -> normal card, ac -> ace card
 * (([c1, c2], [ac1(11), ac2(11)]), 42)
 * (([ac1(1), c1, c2], [ac2(11)]), 28)
 * (([ac2(1), ac1(1), c1, c2], []), 28)
 * Return 28 *)
fun score_challenge (cards, goal) =
    let
        fun loop ((normal, aces), lowest) =
            case aces of
                [] => lowest
              | (suit, _)::rest =>
                let
                    val new_normal = (suit, Num 1)::normal
                    val curr_score = score (new_normal @ rest, goal)
                in
                    loop ((new_normal, rest),
                          if curr_score < lowest then curr_score else lowest)
                end
    in
        loop (separate_aces cards, score (cards, goal))
    end


(* Runs the game and returns the final score of the game after processing
 * (some or all of) the moves in order. Raises the IllegalMove exception
 * for illegal moves. *)
(* TODO: This is incorrect. Write some tests for it and debug. *)
fun officiate_challenge (cards, moves, goal) =
    let
        fun loop (cards, moves, held_cards) =
            case (moves, cards) of
                ([], _) => held_cards
              | ((Discard card)::remaining_moves, _) =>
                loop (cards,
                      remaining_moves,
                      remove_card (held_cards, card, IllegalMove))
              | (Draw::remaining_moves, []) => held_cards
              (* | (Draw::remaining_moves, (suit, Ace)::remaining_cards) =>
                    if sum_cards held_cards + 1 > goal
                    then (suit, Num 1)::held_cards
                    else loop (remaining_cards,
                               remaining_moves,
                               (suit, Num 1)::held_cards) *)
              | (Draw::remaining_moves, first::remaining_cards) =>
                let val min_sum = case first of
                                      (_, Ace) => sum_cards held_cards + 1
                                    | _ => sum_cards (first::held_cards)
                in
                    if min_sum > goal
                    then first::held_cards
                    else loop (remaining_cards,
                               remaining_moves,
                               first::held_cards)
                end
    in
        score_challenge (loop (cards, moves, []), goal)
    end


(* Returns a move list for the given card list and goal. *)
(* NOTE: Score will be 0 when (sum = goal) *)
fun careful_player (cards, goal) =
    let
        (* If it is possible to reach a score of 0 by discarding a card followed by
         * drawing a card, return SOME card otherwise return NONE. *)
        fun discard_check (draw_card, held_cards, goal) =
            case held_cards of
                [] => NONE
              | c::rest => if sum_cards (draw_card::rest) = goal
                           then SOME c
                           else discard_check (draw_card, rest, goal - card_value c)

        (* The loop conditions are as:
         * If there are no cards, return the moves.
         * If the score is 0 (sum of held_cards = goal), return the moves.
         * If the goal is more than 10 greater than the held cards value, draw
         * the next card.
         * Otherwise, do the discard check and if possible make the discard +
         * draw move, else if possible make the draw move, else return the
         * moves. *)
        fun loop (cards, moves, held_cards) =
            case (sum_cards held_cards, cards) of
                (_, []) => moves
              | (sum, c::rest) =>
                if sum = goal  (* Score is 0 *)
                then moves
                else if goal > sum + 10
                then loop (rest, moves @ [Draw], c::held_cards)
                else
                    case discard_check (c, held_cards, goal) of
                        NONE => if sum_cards (c::held_cards) > goal
                                then moves
                                else loop (rest, moves @ [Draw], c::held_cards)
                      | SOME rm_card => moves @ [Discard rm_card, Draw]
    in
        loop (cards, [], [])
    end

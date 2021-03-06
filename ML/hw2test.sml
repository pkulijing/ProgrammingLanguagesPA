(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []
val test1_2 = all_except_option("foo", ["bar", "hi", "foo", "la"]) = SOME["bar", "hi", "la"]
val test1_3 = all_except_option("foo", ["bar", "hi", "la"]) = NONE

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_2 = get_substitutions1 ([["foo", "bar"],["there", "hi"], ["ho", "foo"]], "foo") = ["bar", "ho"]
val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3_2 = get_substitutions2 ([["foo", "bar"],["there", "hi"], ["ho", "foo"]], "foo") = ["bar", "ho"]


val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

		
val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

val test14 = score_challenge([(Hearts, Ace), (Hearts, Ace), (Hearts, Ace)], 20) = 3
val test14_2 = score_challenge([(Hearts, Ace), (Clubs, Num 2), (Hearts, Ace)], 16) = 2

val test15 = officiate_challenge ([(Hearts, Ace), (Hearts, Ace), (Hearts, Ace),(Hearts, Ace)],
  [Draw, Draw, Discard (Hearts, Ace), Draw], 20) = 3
    
val test16 = careful_player([(Hearts, Num 2),(Clubs, Num 8), (Hearts, Num 6), (Hearts, Num 4)], 18)
 = [Draw, Draw, Draw, Discard(Hearts, Num 2), Draw]  

val test16_2 = careful_player([(Hearts, Num 2), (Hearts, Num 4)], 100) = [Draw, Draw, Draw]       
             

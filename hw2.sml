(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2;
	   
(* put your solutions for problem 1 here *)

(* (a) *)
fun all_except_option(option, lst) =
  case lst of
      [] => NONE
    | s::lst' => if same_string(s, option)
		then SOME lst'
		else case all_except_option(option, lst') of
			 NONE => NONE
		       | SOME l => SOME(s::l);
(* (b) *)
fun get_substitutions1(lists, s) =
  case lists of
      [] => []
    | alist::res => case all_except_option(s, alist) of
			NONE => get_substitutions1(res, s)
		      | SOME ans => ans@get_substitutions1(res, s);

(* (c) *)
fun get_substitutions2(lists, s) =
  let fun aux(somelists, acc) =
	case somelists of
	    [] => acc
	  | alist::res => let val resForThisList =
				  case all_except_option(s, alist) of
				      NONE => []
				    | SOME l => l
			  in aux(res, acc@resForThisList)
			  end
  in aux(lists, [])
  end;
(* (d) *)
fun similar_names(lists, {first=x, middle=y, last=z}) =
  let fun get_similars(substitutions) = 
	case substitutions of
	    [] => []
	  | option::res => {first=option, middle=y, last=z}::get_similars(res)
  in get_similars(x::get_substitutions2(lists, x))
  end;
      
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* (a) *)
fun card_color(s, r) =
  case s of
      Spades => Black
    | Clubs  => Black
    | Diamonds => Red
    | Hearts => Red;
(* (b) *)
fun card_value(s, r) =
  case r of
      Ace => 11
    | Num i => i
    | _  => 10;
(* (c) *)
fun remove_card(cs, c, e) =
  case cs of
      [] => raise e
    | acard::res => if c = acard
		   then res
		   else acard::remove_card(res, c, e);
(* (d) *)
fun all_same_color(cs) =
  case cs of
      [] => true
    | c::[] => true
    | c1::c2::res => if card_color(c1) = card_color(c2)
		     then all_same_color(c2::res)
		     else false;
(* (e) *)
fun sum_cards(cs) =
  let fun aux(somecs, acc)=
	case somecs of
	    [] => acc
	  | acard::res => aux(res, acc + card_value(acard))
  in aux(cs, 0)
  end;
    
(* (f) *)
fun score(cs, goal) =
  let val sum = sum_cards(cs)
      val pre = if sum > goal then 3 * (sum - goal) else goal - sum
  in if all_same_color(cs) then pre div 2 else pre
  end;

(* (g) *)
fun officiate(cards, moves, goal) =
  let fun aux(held_cards, cs, ms) =
	case ms of
	    [] => score(held_cards, goal)
	  | (Discard c)::move_res =>  aux(remove_card(held_cards, c, IllegalMove), cs, move_res)
	  | Draw::move_res => case cs of
				  [] => score(held_cards, goal)
				| c::card_res => if(card_value(c) + sum_cards(held_cards) > goal)
						 then score(c::held_cards, goal)
						 else aux(c::held_cards, card_res, move_res)
  in
      aux([], cards, moves)
  end;

(* (challenge) *)
(* (a) *)
fun count_ace(cards) =
	case cards of
	    [] => 0
	  | (c,r)::res => if r = Ace
			  then 1 + count_ace(res)
			  else count_ace(res)
					
fun score_challenge(cs, goal) =
  let val sum = sum_cards(cs)				      
      fun pre(s) =
	if s > goal
	then 3 * (s - goal)
	else goal - s
      fun min(i, j) = if i < j then i else j
      fun minimum_pre(num_aces) = 
	if num_aces = 0
	then pre(sum)
	else min(pre(sum - 10 * num_aces), minimum_pre(num_aces - 1))
  in
      
      if all_same_color(cs) then minimum_pre(count_ace(cs)) div 2 else minimum_pre(count_ace(cs))
  end;

fun officiate_challenge(cards, moves, goal) =
  let fun aux(held_cards, cs, ms) =
	case ms of
	    [] => score_challenge(held_cards, goal)
	  | (Discard c)::move_res =>  aux(remove_card(held_cards, c, IllegalMove), cs, move_res)
	  | Draw::move_res => case cs of
				  [] => score_challenge(held_cards, goal)
				| c::card_res => if (card_value(c) + sum_cards(held_cards) - 10 * count_ace(c::held_cards) > goal)
						 then score_challenge(c::held_cards, goal)
						 else aux(c::held_cards, card_res, move_res)
  in
      aux([], cards, moves)
  end;

(* (b) *)
fun careful_player(cards, goal) =
  let fun find_card(cs, v) =
	case cs of
	    [] => NONE
	  | c::res => if card_value(c) = v
			  then SOME(c)
			  else find_card(res, v)
      fun rev(lst) =
	let fun rev_aux(lst, acc) =
	      case lst of
		  [] => acc
		| x::xs => rev_aux(xs, x::acc)
	in
	    rev_aux(lst, [])
	end
      fun aux(held_cards, cur_sum, left_cards, moves) =
	if cur_sum = goal
	then rev(moves)
	else case left_cards of
		 [] => rev(Draw::moves)
	       | c::card_res => if cur_sum <= goal - card_value(c)
				then aux(c::held_cards, cur_sum + card_value(c), card_res, Draw::moves)
				else case find_card(held_cards, cur_sum + card_value(c) - goal) of
					 NONE => rev(moves)
				       | SOME c0 => aux(c::remove_card(held_cards, c0, IllegalMove), goal, card_res, Draw::(Discard c0)::moves)
  in
      aux([], 0, cards,[])
  end;
      
	
  
  

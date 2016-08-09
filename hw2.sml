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
  

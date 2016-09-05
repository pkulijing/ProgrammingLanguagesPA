(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string;


(**** you can put all your code here ****)			       
(* 1 *)
val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s,0)));

(* 2 *)
val longest_string1 = foldl (fn (s1, s2) => if String.size(s1) > String.size(s2) then s1 else s2) ""; 

(* 3 *)
val longest_string2 = foldl (fn (s1, s2) => if String.size(s1) >= String.size(s2) then s1 else s2) ""; 

(* 4 *)
fun longest_string_helper f  = foldl (fn (s1, s2) => if f(String.size(s1), String.size(s2)) then s1 else s2) "";
val longest_string3 = longest_string_helper (fn (a,b) => a > b);
val longest_string4 =  longest_string_helper (fn (a,b) => a >= b);

(* 5 *)
val longest_capitalized = longest_string3 o only_capitals;

(* 6 *)
val rev_string = String.implode o List.rev o String.explode;

(* 7 *)
fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs => case f x of
		   NONE => first_answer f xs
		 | SOME v => v;

(* 8 *)
fun all_answers f xs =
  case xs of
      [] => SOME []
    | x::xs => case f x of
		   NONE => NONE
		 | SOME v => case all_answers f xs of
				 NONE => NONE
			       | SOME lst => SOME (v@lst);


(* 9 *)
val count_wildcards = g (fn _ => 1) (fn _ => 0);
val count_wild_and_variable_lengths = g(fn _ => 1) (fn s => String.size(s));
fun count_some_var(s, p) = g (fn _ => 0) (fn str => if str = s then 1 else 0) p; 

(* 10 *)
val check_pat =
  let fun get_strings p = case p of
		      Variable x => [x]
		    | TupleP ps => List.foldl (fn (p, acc) => (get_strings p)@acc) [] ps
		    | ConstructorP(_,p) => get_strings p
		    | _  => []
      fun check_duplicate sl = case sl of
				   [] => true
				 | s::sl => if List.exists (fn str => str = s) sl then false else check_duplicate sl
  in
      check_duplicate o get_strings
  end;

(* 11 *)
fun match(v, p) =
  case (v, p) of
      (_, Wildcard) => SOME []
    | (v, Variable s) => SOME [(s, v)]
    | (Unit, UnitP) => SOME []
    | (Const i, ConstP j) => if i = j then SOME [] else NONE
    | (Tuple vl, TupleP pl) => if List.length(vl) <>  List.length(pl) then NONE else all_answers match  (ListPair.zip(vl, pl))
    | (Constructor(s2, v), ConstructorP(s1,p)) => if s1 = s2 then match(v,p) else NONE
    | (_,_)  => NONE;

(* 12 *)
fun first_match v pl =
  SOME (first_answer (fn p => match(v, p)) pl)
  handle NoAnswer => NONE;
  
											   
(* challenge *)
fun typecheck_patterns(cl,pl) = 
  let fun get_type p =
	case p of
	    Wildcard => Anything
	  | Variable _  => Anything
	  | UnitP  => UnitT
	  | ConstP _  => IntT
	  | ConstructorP(s, _) => Datatype(first_answer (fn (s1, s2, t) => if s = s1 then SOME s2 else NONE) cl)
	  | TupleP pl' => TupleT (List.map get_type pl')
      fun smaller_type(t1,t2) =
	case (t1, t2) of
	    (NONE, _) => NONE
	  | (_, NONE) => NONE
	  | (SOME Anything, SOME t) => SOME t
	  | (SOME t, SOME Anything) => SOME t
	  | (SOME IntT, SOME IntT) => SOME IntT
	  | (SOME UnitT, SOME UnitT) => SOME UnitT
	  | (SOME(Datatype s1), SOME(Datatype s2)) => if s1 = s2 then SOME (Datatype s1) else NONE
	  | (SOME(TupleT []) , SOME(TupleT []))=> SOME (TupleT [])
	  | (SOME(TupleT (h1::tl1)), SOME(TupleT (h2::tl2))) => if List.length tl1 <> List.length tl2
								then NONE
								else (case smaller_type(SOME h1, SOME h2) of
									  NONE => NONE
									| SOME t => case smaller_type(SOME(TupleT tl1), SOME(TupleT tl2)) of
											NONE => NONE
										      | SOME (TupleT ts) => SOME (TupleT (t::ts)))
	  | _  => NONE
      fun smallest_type tl =
	case tl of
	    [] => NONE
	  | [t] => SOME t
	  | t::res => smaller_type(SOME t, smallest_type(res))
  in
      smallest_type(List.map get_type pl)
  end
      

 
      

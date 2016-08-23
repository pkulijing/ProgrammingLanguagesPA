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
			       | SOME lst => SOME (v@lst)

						  

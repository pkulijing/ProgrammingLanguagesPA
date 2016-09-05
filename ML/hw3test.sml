use "hw3.sml";

val test1 = only_capitals ["Aaa", "bbb", "Ccc", "Ddd"] = ["Aaa", "Ccc", "Ddd"];

val test2 = longest_string1 ["a", "b", "cc", "dd"] = "cc";
val test2_1 = longest_string1 [] = "";

val test3 = longest_string2 ["a", "b", "cc", "dd"] = "dd";
val test3_2 = longest_string2 [] = "";

val test4 = longest_string3 ["a", "b", "cc", "dd"] = "cc";
val test4_1 = longest_string3 [] = "";
val test4_2 = longest_string4 ["a", "b", "cc", "dd"] = "dd";
val test4_3 = longest_string4 [] = "";

val test5 = longest_capitalized ["A", "bb", "ccc", "Ba"] = "Ba";
val test5 = longest_capitalized ["a", "bb", "ccc", "dd"] = "";

val test6 = rev_string "abcdef" = "fedcba";
val test6_1 = rev_string "" = "";

val test7 = first_answer (fn s => if Char.isUpper(String.sub(s,0)) then SOME s else NONE) ["aaa", "BBB", "ccc", "DDD"] = "BBB";
val test7_2 = (first_answer (fn s => if Char.isUpper(String.sub(s,0)) then SOME s else NONE) ["aaa", "bbb", "ccc", "ddd"]; false)
handle NoAnswer => true;

val test8 = all_answers (fn s => if Char.isUpper(String.sub(s,0)) then SOME (String.explode(s)) else NONE)
	["Aaa", "Bbb", "Ccc"] = SOME [#"A",#"a",#"a",#"B",#"b",#"b",#"C",#"c",#"c"];
val test8_1 = all_answers (fn s => if Char.isUpper(String.sub(s,0)) then SOME (String.explode(s)) else NONE)
	["Aaa", "bbb", "Ccc"] = NONE;
val test8_2 = all_answers (fn s => if Char.isUpper(String.sub(s,0)) then SOME (String.explode(s)) else NONE)
	[] = SOME [];

val test9 = count_wildcards (TupleP [Wildcard, Variable "2", ConstructorP("3", TupleP [Wildcard, Variable "ha"]), ConstP 3]) = 2;
val test9_1 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "ha", ConstructorP("3", TupleP [Wildcard, Variable "ha"]), ConstP 3]) = 6;
val test9_2 = count_some_var("ha", TupleP [Wildcard, Variable "ha", ConstructorP("3", TupleP [Wildcard, Variable "ha"]), ConstP 3]) = 2;

val test10 = check_pat(TupleP [Wildcard, Variable "ha", ConstructorP("3", TupleP [Wildcard, Variable "ha"]), ConstP 3]) = false;
val test10_1 = check_pat(TupleP [Wildcard, Variable "2", ConstructorP("3", TupleP [Wildcard, Variable "ha"]), ConstP 3]) = true;

val test11 = match(Tuple [Constructor("aaa", Const 12),Unit, Const 10], TupleP [ConstructorP("aaa", Variable "b"), Wildcard, Variable "ha"]) = SOME [("b", Const 12), ("ha", Const 10)];
val test11_1 = match(Tuple [Constructor("aaa", Const 12),Const 10], TupleP [ConstructorP("aaa", Variable "b"), Wildcard, Variable "ha"]) = NONE;

val test12 = first_match (Const 10) [UnitP, ConstP 10] = SOME [];
val test12_1 = first_match (Const 10) [UnitP, Variable "a"] = SOME [("a",Const 10)];
val test12_2 = first_match (Const 10) [UnitP, ConstructorP("a", Wildcard)] = NONE;

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

val test13 = typecheck_patterns([], [TupleP [Variable("x"),Variable("y")], TupleP [Wildcard,Wildcard]]) = SOME (TupleT [Anything,Anything]);
val test13_1 = typecheck_patterns([], [TupleP [Wildcard,Wildcard], TupleP [Wildcard,TupleP [Wildcard,Wildcard]]]) = SOME (TupleT [Anything,TupleT [Anything,Anything]]);
val test13_2 = typecheck_patterns([], [TupleP [Variable("x"), Variable("x")], TupleP [ConstP 1, ConstP 1]]) = SOME (TupleT [IntT, IntT]);
val test13_3 = typecheck_patterns([], [TupleP [Variable("x"), Variable("x")], TupleP [ConstP 1, UnitP]]) = NONE;






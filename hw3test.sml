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

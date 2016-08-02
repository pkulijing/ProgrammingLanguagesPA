(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw1.sml";

val d1 = (1991,10,26);
val d2 = (1991,11,26);
val d3 = (1990,10,26);
val d4 = (1991,9,26);
val d5 = (1991,10,26);
val d6 = (1992,9,27);
val dl = [d1,d2,d3,d4,d5,d6];
val ml = [9,10];
val test1 = is_older ((1,2,3),(2,3,4)) = true;
val test1_2 = is_older((1,2,3),(1,2,4)) = true;

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1;
val test2_2 = number_in_month(dl, 10) = 3;

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3;
val test3_2 = number_in_months(dl, ml) = 5;

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)];
val test4_2 = dates_in_month(dl, 10) = [d1,d3,d5];

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)];
val test5_2 = dates_in_months(dl, ml) = [d4,d6,d1,d3,d5];

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there";

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013";
val test7_2 = date_to_string(d1) = "October 26, 1991";

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3;
val test8_2 = number_before_reaching_sum(12, [1,2,3,4,5]) = 4;

val test9 = what_month 70 = 3;
val test9_2 = what_month 334 = 11;
val test9_3 = what_month 274 = 10;


val test10 = month_range (31, 34) = [1,2,2,2];
val test10_2 = month_range(119,123) = [4,4,5,5,5];
val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31);

val test11_2 = oldest([]) = NONE;
val test11_3 = oldest(dl) = SOME(d3);

val test12_0 = remove_duplicates([1,1,2,2,1,3,4,4,5,3,2,4,3]) = [1,2,3,4,5];
val test12 = number_in_months_challenge(dl,[9,9,9,10,10,9,10]) = 5;
val test12_2 = dates_in_months_challenge(dl, [9,9,9,10,10,9,10]) = [d4,d6,d1,d3,d5];

val test13 = reasonable_date(d1) = true;
val test13_2 = reasonable_date(0, 10, 26) = false;
val test13_3 = reasonable_date(1999,13,23) = false;
val test13_4 = reasonable_date(1999,4,31) = false;
val test13_5 = reasonable_date(2000,2,29) = true;
val test13_6 = reasonable_date(1900,2,29) = false;
val test13_7 = reasonable_date(1992,2,29) = true;
val test13_8 = reasonable_date(1992,10,31) = true;




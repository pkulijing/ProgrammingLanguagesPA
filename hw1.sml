fun is_older(d1 : int * int * int, d2 : int * int * int) =
  (#1 d1 < #1 d2) orelse (#1 d1 = #1 d2 andalso #2 d1 < #2 d2) orelse (#1 d1 = #1 d2 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2);

fun number_in_month(dl : (int * int * int) list, m : int) =
  if null dl
  then 0
  else
      if #2 (hd dl) = m
      then 1 + number_in_month(tl dl, m)
      else number_in_month(tl dl, m);

fun number_in_months(dl : (int * int * int) list, ml : int list) =
  if null ml
  then 0
  else number_in_month(dl, hd ml) + number_in_months(dl, tl ml);

fun dates_in_month(dl : (int * int * int) list, m : int) =
  if null dl
  then []
  else
      if #2 (hd dl) = m
      then (hd dl)::dates_in_month(tl dl, m)
      else dates_in_month(tl dl, m);

fun dates_in_months(dl : (int * int * int) list, ml : int list) =
  if null ml
  then []
  else dates_in_month(dl, hd ml) @ dates_in_months(dl, tl ml);

fun get_nth(sl : string list, n : int) =
  if n = 1
  then hd sl
  else get_nth(tl sl, n - 1);

val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];

fun date_to_string(d : int * int * int) =
  get_nth(month_names, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d);

fun number_before_reaching_sum(sum : int, il : int list) =
  if hd il >= sum
  then 0
  else 1 + number_before_reaching_sum(sum - hd il, tl il);
  
val month_lengths = [31,28,31,30,31,30,31,31,30,31,30,31];
fun what_month(i : int) =
  number_before_reaching_sum(i, month_lengths) + 1;

fun month_range(i1 : int, i2 : int) =
  if i1 > i2
  then []
  else what_month(i1)::month_range(i1 + 1, i2);

fun oldest(dl : (int * int * int) list) =
  if null dl
  then NONE
  else
      let fun oldest_nonempty(dl : (int * int * int) list) =
	    if null (tl dl)
	    then hd dl
	    else let val tl_ans = oldest_nonempty(tl dl)
		 in
		     if is_older(tl_ans, hd dl)
		     then tl_ans
		     else hd dl
		 end
      in SOME(oldest_nonempty(dl))
      end;

fun remove_duplicates(il : int list) =
  let fun contains(il : int list, i : int) =
	if null il
	then false
	else hd il = i orelse contains(tl il, i)
      fun select_existing(il : int list, res : int list) =
	if null il
	then res
	else
	    if contains(res, hd il)
	    then select_existing(tl il, res)
	    else select_existing(tl il, res@[hd il])
  in
      select_existing(il, [])
  end;				      

fun number_in_months_challenge(dl : (int * int * int) list, ml : int list) =
  number_in_months(dl, remove_duplicates(ml));

fun dates_in_months_challenge(dl : (int * int * int) list, ml : int list) =
  dates_in_months(dl, remove_duplicates(ml));

fun reasonable_date(d : int * int * int) =
  if #1 d < 1 orelse #2 d < 1 orelse #2 d > 12 orelse #3 d < 1
  then false
  else
      let fun leap(y : int) =
	    (y mod 400 = 0) orelse (y mod 4 = 0 andalso y mod 100 <> 0)
	  val d_false_2_nonleap = #2 d = 2 andalso not (leap(#1 d)) andalso #3 d > 28
	  val d_false_2_leap = #2 d = 2 andalso leap(#1 d) andalso #3 d > 29
	  fun get_nth(il : int list, n : int) =
	    if n = 1
	    then hd il
	    else get_nth(tl il, n - 1)		
	  val d_false_no2 = #2 d <> 2 andalso #3 d > get_nth(month_lengths, #2 d)
      in
	  not (d_false_no2 orelse d_false_2_nonleap orelse d_false_2_leap)
      end;
  

				     
				     


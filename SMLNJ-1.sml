

(* SML comments appear like this *)

(* Evan Holmberg *)

(* #1 - pow *)
(* takes 2 ints as parameters *)
(* checks if the power is 0 and returns one because x to the power of 0 is 1 and if not then calls the function again with the same a value but the power gets decremented *)
fun pow (a, b) = if b = 0 then 1 else a * pow (a, (b-1));

(* #2 - sumTo *)
(* checks if the parameter is zero and returns 0.0:real if it is, otherwise 1.0 gets divided by the real value of int and gets added to the recursive call of *)
(* the function consisting of 1 less of the original parameter *)
fun sumTo x = if x = 0 then 0.0 else 1.0/(Real.fromInt(x)) + sumTo(x-1);
               

(* #3 - repeat *)
(* takes 2 parameters, the string and an int, if the int is 0 then an empty string is returned, otherwise the string is *)
(* concatenated with the recursive function call which parameters consist of the original string and 1 less of the original int n value *)
fun repeat(s, n) = if n = 0 then "" else s ^ (repeat(s, (n-1)));
 

(* #4 - binary *)
(* Takes 1 parameter and if it is equal to zero then then the string binary representation of zero is returned, otherwise the function is *)
(* called again recursively on the original parameter but divided by 2 and then that is concatenated with the string representation *)
(* of the original parameter x modulo 2 because an int mod 2 is either going to be 1 or 0 *)
fun binary x = if x = 0 then "0" else (binary(x div 2)) ^ (Int.toString(x mod 2));


(* #5 - countNegative *)
(* takes a int list as a parameter and if it is empty then 0 is returned, otherwise the head of the list is compared to 0 and if it is *)
(* then 1 is added to the recursive call of this function which is applied to the tail of the original list, if the head of the *)
(* list is not greater than 1 however, the function is just called recursively on the tail of the original list to check to see if the next int in the list is negative or not *)
fun countNegative x = if null x then 0 else if (hd(x)) < 0 then 1 + countNegative(tl(x)) else countNegative(tl(x));
 

(* #6 - absList *)
(* Helper function which takes a tuple int * int as a parameter returns a tuple consisting of the absolute values of the elements of the original *)
fun absHelper (i : int, j : int) = (abs(i), abs(j));

(* Takes a list of tuples of integers as a parameter and if it is empty then an empty list is returned otherwise the *)
(*absHelper function is called on the first tuple in the list which is concatenated with the recursive call to the tail of the list *)
fun absList ([]) = [] | absList(x) = [absHelper(hd x)]@absList(tl x);

(* #7 - split *)
(* takes an list of integers as a parameter and if the list is empty then an empty list is returned, otherwise creates a tuple consisting of the *)
(* floor of the head of the list divided by 2 and the ceiling of the head of the list divided by 2, this order is important because this ensures the first index of the tuple is smaller *)
(* than the second index of the tuple, if applicable. Then the tuple is concatenated with the recursive function which uses the tail of the original list as an argument resulting in a *)
(* list of int * int tuples *)
fun split ([]) = [] | split(x) = [(floor(real(hd x)/ 2.0), ceil(real(hd x) / 2.0))]@split(tl x);
 
(* #8 - isSorted *)
(* takes a int list as a parameter and if it is of length 0 or 1 then it is considered sorted, otherwise the head of the list is compared to the head of the tail of the list *)
(* and if it is less than or equal to then the function is recursively called with the tail of the list as a parameter until the length of the list is 1 which again would return *)
(* that it is true the list is sorted, otherwise then false the list is not sorted *) 
fun isSorted x = if null x orelse length(x) = 1 then true else if hd x <= hd(tl x) then isSorted(tl x) else false;

(* #9 - collapse *)
(* takes a list as a parameter and if it is empty then an empty list is returned, otherwise, if there is only one element in the list then a list containing that *)
(* element is returned, otherwise the head of x is added to to the head of the tail of x and then concatenated with the recursive call which takes the tail of the tail of the list x *)
fun collapse [] = [] | collapse([x]) = [x] | collapse(x) = [hd(x) + hd(tl(x))]@collapse(tl (tl x));

(* #10 - insert *)   
(* takes an int and a int list as parameters and if the list is empty then a list containing the int n variable is returned, otherwise if n is greater than the head of the list x *)
(* then the head of the list x is appended to the recursive call of the function which has n as an argument again and the tail of the original list, if it is not greater, then n is *)
(* appended to x, this function results in a list of ints being returned *)    
fun insert(n, []) = [n] | insert(n,x) = if n > hd x then (hd x)::insert(n, tl x) else n::x;
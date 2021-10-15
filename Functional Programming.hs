---- Part 1a ----

{-
One of the major advantages of functional programming is the primary usage of ‘Pure Functions.’ Pure functions
will always produce the same output without having any other variables or values changing its result. The 
entire program is incredibly easy to debug with functional programming as you need only find what to change in that
specific function rather than worrying about other portions of your code affecting the results. 

Lazy evaluation is something that is present in functional programming languages (such as Haskell) and can be a nice benefit to 
have. Functional programming only stores values when it is necessary. By doing this it doesn’t have to repeatedly evaluate the
functional programming inputs and does it all at the when storing it. While this can lead to slight slow down in some 
scenarios (in a constant factor) for the most part the program it may be sped up due to it asymptotically as it wont be
constantly re-evaluating the function. This really helps when programs reach a certain size because the flat performance
reduction from initially doing it may be small but the performance gains from not repeatedly running it can be a massive 
benefit. 

The lack of assignment statements in Functional Programming can be considered as beneficial when compared to 
other programming languages. This means you wont have random side effects that you may not have foreseen due to values
not actually changing throughout the program, however this can be a common, hard to diagnose problem in many other 
non-functional programming languages. This is known as referential transparency and examples are as follows:

	Take variable A as A = 1
	A = A + 1 will produce 2 on the 2nd and then 3 on the 3rd attempt
	A now = these new values and has been changed so it is not referentially transparent

If we instead had a function   Int -> Int where the input was A and the output was A+1, the value of A itself hasn’t
changed however it has achieved the goal required without losing the initial value of A (which can be very useful if 
you need to use things like initial inputs again in the end of it)
	


Sources:
For 1A I used:
https://medium.com/@devisha.singh/7-unbeatable-advantages-of-functional-programming-b5d1af1edbe1 - For some ideas to general advantages of Haskell.
https://en.wikipedia.org/wiki/Functional_programming – I used the wiki to get a greater understanding of what many of the advantages meant and expand upon them.

-}







---- Part 1b ----

{-

A mathematical function is a type of relation that uniquely maps all members of a set with all combinations of members of a different set.
Every single input (e.g. x) must have an output value associated to it (e.g. y). This means you cannot have a valid input that does not
give an output. They can have different properties, injective (all elements of the output set are mapped to an input), surjective (all
elements inputted, have a unique output not achieved by a different input) and bijective (both injective and surjective). Not only that
but functions can also be recursive in mathematics, for example, the Fibonacci sequence which takes a natural number input, if its 0 it
does nothing but otherwise it calls itself with n-2 + n-1.

Now we can consider a Haskell function instead. In Haskell, a function is a mapping of values of one type into a different type. While the 
application of the function can be slightly different (e.g. mathematically it is f(x) and in haskell it is f x among some other application
 changes) they mostly achieve the same goal and hence resemble each other incredibly closely). Haskell functions take a value/set of values
 and have one output, this is very similar to a mathematical function. The signature in Haskell can represent the set of inputs and outputs,
 just like the sets in mathematical functions (e.g. [Int] is the set of all inputs and in maths N is the set of all natural numbers). Functions
 can also have the same properties (injective, surjective and bijective). For example in haskell and maths, a function that takes an input x
 and outputs 2x + 1 would be injective. A function that took A and B and outputted A+B would be surjective as A and B could be different values
 and yield the same results and a function that took A and outputted B would be bijective. Last but not least, recursion is also very common in Haskell functions as you can take a function and make it call itself (e.g. input x = 0 if 0 otherwise x + input x). As you can see the functions in Haskell are pretty much the same as the functions in maths in all but their application (how they are written). In other words, a Haskell function is basically a mathematical function but put into code so in the end, they have an incredibly high resemblance.

Sources:
I just used knowledge of the Mathematics module I have learnt since being at uni
-}







---- Part 1c ----
{-

A higher order function is a function that uses a different function as its input. For example, see the case below. Multiply works alone, you can 
do Multiply 3 4 to get 3*4 = 12 and any other integer value you could think of however we have another function doubleHigher. Alone, doubleHigher
doesn’t do anything however when you plug a function into it with only 1 of its original arguments (e.g. higherOrder(multiply 3), it takes
multiply 3, and plugs it into its own function, which becomes    x 2 where x is the input so 3 2. It then plugs 3 2 into multiply, and does 
multiply 3 2, which = 3 * 2. Essentially the higher order function uses the other function to help multiply the function and its input by 2.
If I had a similar function to multiply, e.g. addition, I could use higherOrder(addition x) to do x + 2 as well.	

-}


multiply::Int->(Int->Int)
multiply x y = x * y


higherDouble::(Int->Int)->Int
higherDouble x = x 2








---- Part 2a ----
{-
This code works by first calling the function steps. When the function is called with 3 integers, p m and n it will check if any of them are 0.
If any of them are 0 it will proceed to return an empty string (because logically if any are 0 it would be 0 in all dimensions). If they arent
0 it will call a where clause 'go' with the variables p m and n. P and n will have the same definition here however m is not used and instead we
us z as we want to look back on m in the function later without it reading a different input value as m. The go clause first checks to see if 
n = 0. Because that check was done in the beginning already it wont start as that but could become that as its called again. If n is not 0 then
it will do replicate z "*" which replicates '*' z times. It the does concat on this function and "\n" which turns the list of elements * and the
string "\n" into one big string added together. It then replicates this p many times because it is p steps high and concats again to make it one
big string again. After doing this it calls itself recursively but with p and z+m to add m to the height it goes up by and then n-1 to represent
the amount of times it does this when n = 0 it will then call init(og p m n). Init removes the last character from og, which will always be a "\n"
to get rid of the extra line after and og p m n, is called og, because it is the reverse of go. It will proceed to do the same steps above but with
the recursive part first to start off big and end small. When this is called with putStr the "\n"'s are interpreted as new lines and make the
pattern work.
-}

steps::Int->Int->Int->String
steps p m n
 |p == 0 || m == 0 || n == 0 = ""
 |otherwise = go p m n 
 where
   go p z j | j == 0 = init(og p m n)
            | otherwise = concat(replicate p(concat(replicate z "*")++"\n"))++ go p(z+m)(j-1)
   og p z n | n == 0 = ""
	    | otherwise = og p (m+z) (n-1) ++ concat(replicate p(concat(replicate z "*")++"\n"))





---- Part 2b ----
{-
This is similar to 2a but slightly more annoyingly complicated. It only takes  2 inputs, n and m. With these 2 inputs n is the length of the flag
and 3 is the amount that are printed. It checks to see if n or m are = to 0 and if they are it produces an empty string, if not it will check to
see if the value of n `mod` 2 == 0. This is checking to see whether the value of n inputted is odd or even. If it is even it will first use the 
concat replicate feature m times to replicate the amount of times it does this. So if it is 3 times, it will repeat the flag pattern (code inside
brackets) 3 times. It will then concat and replicate "*" n times for the top half of the flag and then add "\n" to make a newline. Next the where
clause go is called with n `div` 2 twice. This go clause checks to see if n = 1, if it does blank space, if not, it will check to see if n is even
or odd and will act accordingly. If even it will print a * for the first part, then a certain amount of blank spaces (with concat(replicate(y-x)(" ").
Next it will add on another *, representing the * in the diagonal position on the flag before finishing it off with a justified amount of spaces 
that work with x - 2. This is currently in the middle of the flag. It then does the same but in reverse to finish off the other side of the flag.
After doing this it will then call go again for x-1 which will repeat the go command. It will do the exact same thing and not be odd because n is 
not recalled and n is still even in the actual program. It now comes out of this and adds on og(n`div`2)(n`div`2), which similarly to the last one does
the same stuff but in a slightly different order which causes it to print the bottom half. It then closes up with concat(replicate("*")) printing the
bottom of the flag. If the originally input n was an odd number then it will do a very similar thing but start off sending in (n-1)`div`2. This is just
for clarity and understanding in the program to avoid confusion but due to the way div handles odd numbers we didnt actually need the -1. The difference
is now that its odd it adds and extra line in the middle which is what the code inbetween the go and og functions do. They are pretty simple and just
add a *, then div 2 length - 1 then the * slap bang in the middle, then the other div 2 length of flag - 1 then a * at the very end to finish it. 
It is also worth noting that when odd, the go (and consequently og) functions will be slightly different in the fact that they add a space in the middle 
to compensate for the middle row being added. Again, putString interprets "\n" as newlines and turns the string into a nice bunch of flag patterns.
One final thing to consider is the init functions being used in both cases. This is to get rid of the last "\n" so there isnt a blank line under the
flag pattern

-}


flagpattern::Int->Int->String
flagpattern n m 
 |n == 0||m == 0 = "" 
 |n `mod` 2 == 0 = init(concat(replicate m(concat(replicate n("*"))++"\n"++go((n`div`2))((n`div`2))++og((n`div`2))(((n)`div`2))++concat(replicate n("*"))++"\n")))
 |otherwise = init(concat(replicate m(concat(replicate n("*"))++"\n"++go(((n-1)`div`2))(((n-1)`div`2))++"*"++concat(replicate ((n`div`2)-1)(" "))++"*"++concat(replicate ((n`div`2)-1)(" "))++"*"++"\n"++og(((n-1)`div`2))(((n-1)`div`2))++concat(replicate n("*"))++"\n")))
 where
   go x y| x==1=""
 	 | n `mod` 2 == 0 = "*"++concat(replicate (y-x)(" "))++"*"++concat(replicate (x-2)(" "))++concat(replicate (x-2)(" "))++"*"++concat(replicate (y-x)(" "))++"*"++"\n"++ go (x-1) y
	 | otherwise = "*"++concat(replicate (y-x)(" "))++"*"++concat(replicate (x-2)(" "))++" "++concat(replicate (x-2)(" "))++"*"++concat(replicate (y-x)(" "))++"*"++"\n"++ go (x-1) y
   og x y| x==1=""	 
         | n `mod` 2 == 0 = "*"++concat(replicate (x-2)(" "))++"*"++concat(replicate (y-x)(" "))++concat(replicate (y-x)(" "))++"*"++concat(replicate (x-2)(" "))++"*"++"\n"++ og (x-1) y
	 | otherwise = "*"++concat(replicate (x-2)(" "))++"*"++concat(replicate (y-x)(" "))++" "++concat(replicate (y-x)(" "))++"*"++concat(replicate (x-2)(" "))++"*"++"\n"++ og (x-1) y









---- Part 3 ----
{-
For part 3, 2 strings are taken as inputs to return 1 string as an output. The 2 inputs are taken and the output string starts off with a,
the first inputted name, it then calls the function lphi with the input being the string produced by function everyLetter. This everyLetter 
strings input is 0, and then a the second string without any spaces (which is what the next list function does, it takes every element that is
not a space and uses it, hence getting rid of all thespaces in an input), then the 3rd input to everyLetter is the 1st string without any spaces.
everyLetter then proceeds to do i a b . First it checks length. If b = i, then it will return a if not it will call itself again, with i + 1, b but
then call compatiletter for the current letter of b, and the word a. Compatiletter will then check to see if the current character is in a. It does 
this by checking if the length of the string is = to i, if it is then that is the output, else, it will check if the current letter of a, is equal
to the input of letter c. If not it takes the head, adds it to the string then recursively calls the tail but with i + 1 to update the position. It
does this until I is the length of the string and if that is the case then it will return the full string. If at any point it notices a character is
the same it will take the first i characters of the string, then the last i-1 characters (by dropping the first i+1 characters). This removes the 
character that was the same from the string. After we have this compatiletter returns to everyLetter. Since everyLetter is recursively called it 
moves on to the next letter and then does the same again to see if that character is returning. In the end a string is returned of a, but with the
characters that were the same removed (e.g. if a = "aab" and b ="ab" it would return a), this is equivalent to *ing out the letters to show they are
the same. It then does LPHI on this and does a rotation where if its length is divisible by 4 it will get the string "indifferent", if modding returns
1 it is "loves", if it is 2 it is "phsyical" and if it is 3 it is hates. We then add on the b ++ "and" ++ b string and before doing the exact same 
thing again, but with the original string of b, comparing it do a. This does the exact same everyLetter (and hence compatiletter) for b and removes 
all of its duplicate letters, before it ends up with the new value of the string and doing an LPHI rotation on it to find out what string b feels about 
string a. It then adds on the name of a to the end to show who its towards giving us the output string and we are done :)

-}


compatibility::String->String->String
compatibility a b = a ++ lphi(everyLetter 0 [ x | x <- b, not (x `elem` " ")] [ x | x <- a, not (x `elem` " ")]) ++ b ++" and "++ b ++ lphi(everyLetter 0 [ x | x <- a, not (x `elem` " ")] [ x | x <- b, not (x `elem` " ")]) ++ a

everyLetter::Int->String->String->String
everyLetter i b a
 |length b == i = a
 |otherwise = everyLetter (i+1) b (compatiletter 0 (b !! i) a)

compatiletter::Int->Char->String->String
compatiletter i c s
 |length s == i = s
 |s !! i == c = take i s ++ drop (i+1) s
 |otherwise = compatiletter (i+1) c s

lphi::String->String
lphi a
 |(length a) `mod` 4 == 0 = " is indifferent to "
 |((length a) `mod` 4) == 1 = " loves "
 |((length a) `mod` 4) == 2 = " wants to get physical with "
 |otherwise = " hates "







---- Part 4 ----
{-
We start off with a polymorphic function 'lsplit.' This function takes in a value of lists a (could be a string a because of how haskell interprets
strings). We use Eq a to constrain the set types we are using for the function so it doesnt get confused. When lsplit is called with 2 input's xs and
a, it calls the function subLength with an input of the function split(xs a). Split xs a, takes the inputs and checks to see if the list/string input 
xs is empty. If it isnt, it then does takewhile for y which returns all elements of the list (/chars of the string) until y appears (or the whole thing
if y is non-existant. It then adds on split of drop of length of takewhile +1. The drop length part removes the first part of the takeWhile that has 
already been taken and 1 more to get rid of the y value itself. The split function sends the same thing through again but what was cut off from the list
originally. This is being recursively called and it keeps happening adding on the values of takewhile, but to a list, creating a list of the values but 
all split up. When this comes out of the output of the function subLength is initiated on this list of lists. If the list is empty, subLength returns an 
empty list. if not it takes the head and tail of the list. It checks to see if the head is 0, if it is it just does subLength on the tail of the list. If
not then it will add the length of x (which is this sublist) onto a new call of subLength taking the tail of the list. What this function essentially does 
is it creates a list, of the lengths of the lists within it but if the lists are empty lists, it doesnt include that as 0 (just banishes it). This function
is now done
-}




lsplit::Eq a =>[a]->a->[Int]
lsplit xs a = subLength(split xs a)


split::Eq a =>[a]->a->[[a]]
split [] y = []
split xs y = [takeWhile (/= y) xs] ++ split (drop (length(takeWhile (/= y) xs)+1) xs) y
   

subLength::Eq a =>[[a]]->[Int]
subLength [] = []
subLength (x:xs) 
	|length x == 0 = subLength xs 
	|otherwise = [length x] ++ subLength xs 






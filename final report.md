# Final Project Report: BAPL (Basically Another Programming Language) by Martin Friesen

BAPL is the acronym of the course [Build A Programming  Language](https://classpert.com/classpertx/courses/building-a-programming-language/cohort) and the working codename of my language BAPL (Basically Another Programming Language). It is a tribute to the course in general which, in a word, was a blast! It also highlights the course's emphasis on recursion and reuse of patterns.

My language recursed when, in **Week 8**, it redid *8 Queens Ex 2.1 of Programming In Lua*, the homework of **Week 1**. It was a refreshing experience since my language added a few nice touches: 'forward function declaration', 'unless', 'string interpolation', and for good measure, a royal anthem. 

	PROGRAM ----------------------------- 137 ------------------------------
	001 #{
	002   where BAPL began with chapter 1-2 of PILua book Ex 2.1 ...
	003   enhanced with 'forward function declaration', 'unless', 'string interpolation', 
	004   and a royal athem
	005 }#
	006 
	007 function addqueen(rows_col, cur_row);
	008 
	009 function main () {
	010   var N = 8;
	011   print_first_soln_only = 1;
	012   isa_soln_printed = 0; # to stop after printing first solution
	013   var qs = new[N];
	014   addqueen(qs,1);
	015   return "God Save the Queen."
	016 }
	017 
	018 function isplaceok (rows_col, cur_row, col) {
	019   for row = 1, cur_row - 1 { # for each queen already placed
	020     var previous_queen_col = rows_col[row];
	021     if (previous_queen_col == col or                    # same column
	022        (previous_queen_col == col - cur_row + row) or   # same diagonal to left
	023        (previous_queen_col == col + cur_row - row) ) {  # same diagonal to right
	024         return 0
	025     }
	026   };
	027   return 1
	028 }
	029 
	030 function printsolution (rows_col) {
	031   var s = "\n";
	032   for row = 1, array.len(rows_col) {
	033     for col = 1, array.len(rows_col) {
	034       #s = "{}{} "  <- s  <- (rows_col[row] == col and "X"  or  "_")
	035       s = "`s``rows_col[row] == col and 'X'  or  '_'` " # latest `backticks` alternative (but, above still works too)
	036     };
	037     s = "{}\n" <- s;  
	038   };
	039   @ s
	040 }
	041 
	042 function addqueen(rows_col, cur_row) {
	043   if cur_row > array.len(rows_col) {
	044     printsolution(rows_col);
	045     isa_soln_printed = 1
	046   } else {
	047     for col = 1, array.len(rows_col) {
	048       if isplaceok(rows_col, cur_row, col) {
	049         rows_col[cur_row] = col;
	050         unless print_first_soln_only and isa_soln_printed {
	051           addqueen(rows_col, cur_row + 1)
	052         }
	053       }
	054     }
	055   }
	056 }
	
	
	printing: 
	X _ _ _ _ _ _ _ 
	_ _ _ _ X _ _ _ 
	_ _ _ _ _ _ _ X 
	_ _ _ _ _ X _ _ 
	_ _ X _ _ _ _ _ 
	_ _ _ _ _ _ X _ 
	_ X _ _ _ _ _ _ 
	_ _ _ X _ _ _ _ 
	
	
	--->God Save the Queen.


Test cases, from 1 to 137 seen above, tell the story of what was for me a wonderful journey of creating a programmming language from the "ground up", although of course it was really from "standing on the shoulders of giants". It is hard for me to imagine a better way to experience this than what was provided by the course. 
## Language Syntax

The overall syntax of my language follows Selene, the language developed in the course.   

Selene adopts curly braces for blocks, and otherwise chooses fairly conventional syntax choices. A trade off of using curly braces for blocks is that it "robs" the opportunity for them to be used for other things such as array initialization. Sure, they *could* be used, as expressions. but they would not stand out as much. It would probably take extra effort to parse given crosstalk with blocks. This is one reason to appreciate Lua's choice of then/do/function ... end structure.

Square brackets seem like a natural choice for array indexing. The question arises, should they also be used to index strings, that can be viewed as a kind of one dimensional array? I shy away from that in favor of all things string related having their own look and feel. For that I chose the '\~' operator, that looks like a string as much as any standard symbol. I trust that helps a single glance at a code base to see what work is being done more quickly and clearly. Especially if array indexing is mixed with string indexing to compose complex operations.

Selene requires semicolons to separate statements, except for the last one in a sequence. This is something that could be cleaned up, but it is easy enough to live with for now.

Exceptions:
1. a prefix operator 'not' rather than '!' for negation

    '!' was used for a suffix left associated factorial operator (see new features below)
    
    this is a pseudo exception since Selene doesn't have factorial, but '!' was suggested for negation
2. 'and' is higher priority than 'or'  

    this avoids needing parenthesis is some cases  
    both implemented as shortcut operators  
    another pseudo exception as this is an addition
3. relational operators are implemented mathematically  

    a < b < c evaluates to a < b  and  b < c
4. 1 <-2 is not valid
    * this is due to my addition of the '<-' operator for string interpolation
    * use 1 < -2 instead
    
## New Features/Changes

My language adds numerous features beyond those of Selene. I'll cover them in order of being added.

Detailed examples of all the features described below are given in the 136 test programs I include with the project. A few examples are also sprinkled in the explanations below for ease of reading. Various trade-offs and limitations are noted along the way.

1. number styles
    * hexadecimal  
    * scientific  
        * how large or small a number can be depends on the vm which is Lua based  
        * e.g. using Lua compiled to 32 bits is not very good for times spanning many decades  
        * no overflow checks are provided - user beware  
    
    * 0 and 1 are overloaded so to speak, and are used to represent false and true  
        * however, anything but 0 can represent true in conditions  
        this makes some algorithms easier to implement e.g. 
            looking for a countdown to 0 in a while condition  
            
                "The Nothing That Is", A Natural History of Zero
                cites how for the longest time 0 was not even considered a number  
                not surprisingly then it can serve well for false 
                
                
                however, in some contexts, 0 can be considered a true value  
                * e.g. for generator zones, offline can be represented by 0 MW,  
                but so can being online at 0 MW
                
                modelling these can be a tricky affair with various tradeoffs
        * limitations: 
            * no Boolean type was added
            * no automatic conversion between floating point and integers is provided
            * the Lua based vm dictates what happens  
            * e.g. exponentiation and division create a floating point number
    * hexadecimal and scientific formats can be inputted, but output format of numbers depends on Lua
    
2. additional operators  
    * '+-*/' Selene provides basic left associative math operators  
            no trapping of divide by 0 or arithmetic overflow  
    * I added:  
        1. '!' multiple left associative factorial operator  
            e.g. (2+1)!! gives 720  
        2. '-' unary minus  
            can be used repeatedly e.g. - -2 gives 2  
            is not confused with binary minus e.g. 2 - -2 gives 4  
            can operate on parenthesized expressions e.g. 1 - -(x + y)  
        3. '%' left associative remainder operator  
        4. '^' right associative exponentiation  
        5. '<' '<=' '==' '=>' '>' relational operators  
            binary associative; later I changed the semantics to be mathematical; see below  
        6. other operators were added later and are discussed below  
        7. used pcall to trap runtime div by 0 and other errors arising in the vm  
        8. limitations:  
            * no transcendental functions  
            * no rounding functions,  
            * no biggest/smallest number functions  
            * however, the standard library function framework I began would supply them  
                * the first example of that is **array.len** builtin demo'd in 8 Queens

3. zone type  
    * the zone literal is expr : expr where ':' operator brings the expr's together in a table to make a zone  
            a metatable is set on the table to overload basic math operators that make sense for zone  
            limitations: expr are assumed to be numbers; other values will result in runtime errors  
                adding types to my language would enable catching more such errors at parse time  
    * examples: -3:8, x:2, (x+3):(y-1)  
            a late realization was that zone could be used to implement a string substr and trim  
            I don't know Python but lately saw it uses something similar e.g. mystr[3:4]  
            see string feature below for further discussion of how I used zone for substr and trim  
    * only unary +/- have higher precedence before numbers than zone  
    * this enables zone literals to have negative numbers without using parentheses  
            tradeoff is that -(x:y) is required for negating a whole zone (rarely if ever needed)  
    * zone participates in basic operations numbers do (zone has a metatable that implements those operators)  
            e.g. 3:4 + 5 gives 8:9  
            my tests give many more examples  
    * adding the zone type  
            deepened my understanding of material being taught (Lua, LPeg, and design choices)  
            enabled me to return multiple results (enhanced testing)  
            increased enjoyment since the language could express more  
    * tradeoffs  
            a little more baggage early on, but a little went a long way to enhance learning  
    * overloading  
        * a metatable was used to implement math operations for zone's  
        * as such the metatable seemed like a concept holder (math, print, concat operations)  
        * the math and print operations could reasonably be overloaded for zone  
        * but I could not overload my factorial operator in a similar way  
                this seems to be a limitation of metatables  
        * this raised the question of what metatables might do  
            it seems it would be nice to have separate metatables for separate concepts  
            e.g.  
            * math operations  
            * string operations  
            * zone operations  
            * that can be done to some extent by implementing a subset of the operators
            * could a VisiCalc way of BAPL entail:  
                * adding symbols, their associativity, binary/unuary, precedence  
                    having BAPL modify the grammar, i.e. is there a grammar of grammar?  
                    * allow the symbols to become available for others to overload e.g. colon = ':'  
                        {__colon = function(a, b) return tozone(a,b) end, ...}  
                    * a kind of extendable metatable  
                * we started to add node("tag", "etc")  
                    * could other constructs automate adding new binary operators and semantics?  
                    * could we achieve a kind of VisiCalc grammar of grammar, sometimes called language workbenches?
    * inheritance by attributes  
        * the 'size' attribute was used by my table implementation of arrays  
             * just by adding the 'size' attribute to my table implementation of zones, my zones automatically inherited all behaviors of arrays!  
             * for a moment I felt like the Doc in *Back to the Future* reflecting on his flux capacitor  
             invention : "Marty! What have I done?!"  https://www.youtube.com/watch?v=uYd8CWiCsdk  
            * I'll keep this phenomenon as an idea for, ahem, future exploration  

4. shortcut 'and' and 'or'  
    * these return arbitrary values rather than just truth values 0 or 1  
        * they enable efficient ternary expressions as long as expr2 isn't 0  
            * e.g. expr1 and expr2 or expr3 gives if expr1 {expr2} else {expr3}  
            * my 8 Queens case demos this

5. elseif  
    * this was very challenging for me to do using the grammar  
        * initially I used my own recursive functions to create the AST  
        * then I morphed the functions to one elegant function, to look like something the grammar might do  
        * finally I used the grammar so it did the same recursion and nesting per my manual function  
    * a case statement would have been nice to add, but if,elseif,else like while is fundamental  
        * I did add 'for' though to augment 'while' (see 18 below)  
           * wait ... did I just do a **fix2here** with the above reference?  

6. relational operators implemented mathematically  
    * i.e. a < b < c evaluates to a < b  and  b < c  
    * used short cut 'and' and 'or' for efficiency  
    * BTW: a habit I picked up from Bjarne Stroustrup is consistently expressing relations with the least value on the left  
        * e.g. a < b versus b > a  
        * it imparts a nice order to the code and can speed up readability/comprehension  

7. relative jumps to allow bytecode to be moved as needed  
    * in retrospect it may have been better to make jumps relative later to make testing easier at first
    * vm questions  
        * jumps are primitive byte code operations mimicking low level machine operations  
        * but, my vm used high level Lua to implement arrays and garbage collection for example  
        * I imagine a real world vm would be written in C++ and similarly use high level C++ features  
            * eventually things hook to compiled machine code one way or the other  
        * so, I have a hybrid of low level and high constructs in my vm  
        * since I aim for DSLs, do I even need jumps etc, or should I rather transpile to Lua or C++?
    tradeoffs/limitations  
        * how parallel processing friendly is my vm?  
        * how GPU friendly?  
        * how amenable is the vm to adding graphics primitives?  

8. inc, dec, not prefix ops for incrementing and decrementing numbers and negation  
    * 'not' is very low priority so it can apply to relational results  
        * e.g. not a < b < c gives not (a < b < c)  
    * similarly unary - is done before binary +-*/  
    * together with 'and' being higher priority than 'or' and given the 'unless' statement (see 12 below) enables some clean looking expressions  
        * e.g.  
            * 8 Queens ternary expression for printing a solution 
            * for loop implementation  
    * tradeoffs:   
        * dec and inc operaters interspersed with repeated unary - might make some heads spin  
        * ++ and -- operators could still be added enabling post and pre operations on variables  
        * mine don't change variables e.g. x = 1; inc x + x gives 3  
            * it looks like C++'s x++ + 1, but does not change x  
            * the separation (or not) of the operators from the variables suggests the actual semantics in each case  
            * inc dec have the advantage of being able to be used on what C++ would call rvalues  
            * ++ post or pre have the advantage of being able change variable values  

9. syntax error reporting
    * used match time functions early on to capture ID's and give precise syntax errors
        * this would eventually enable reporting to users even what would seem to be calling a function but without brackets
            * e.g. function myfunc(); ... x = myfunc;
            * ..........................^
            * no such variable named myfunc
            * you may have intended to call myfunc()
    * flowed syntax errors to end so the language could be run repeatedly on different tests without halting
    * array bounds checking at runtime  
    * duplicate function or var definitions checking  
    * reporting attempts to index a non-newed array or uninitialized variable  
    * examples of these are given in the test runs  
    * limitations  
        * further work is needed to give better syntax error messages for missing braces semicolons etc  
            * however, I have a framework in how I reported duplicate forward function declarations
        * in the meantime using the max position of parsing goes a fair distance  

10. use of runtime memory variables to do jump decisions
    * was not required, but I learned about what might be entailed in providing more dynamic features  
        * e.g. loading code at runtime
    * creative coding gone wild
    * removed from final implementation
11. string type  
    * I added this early on in simple form  
    * it felt like imparting the ability for my new baby to speak   
    * it was a joy to hear it's first words, and it helped a lot in debugging  
    * I added to it progressively through the course  
    * interpolation especially enhanced debugging  
        * e.g. "i={} j={}" <- i <- j; giving e.g. "i=1 j=2"  
        * '<-' the arrow-like operator denotes expressions flowing into the string to fill the slots denoted by {}'s  
        * {}'s are paired with expr's in way that accommodates too many or too few expr's  
        * it felt like a step toward implementing multiple return values  
            * nuance  
                * when feeding string slots '{}', an array or association is assumed  
                *  however, why not allow a mix of arrays and scalars?  
                * if so exhaust arrays and then move to next given entity?  
        * as an expression it can be composed with additional layers of interpolation  
            * similarly with other string operations (see concat, substr etc below)  
        * I will consider using '<<' instead
            * easier keyboarding
            * pops out more
            * doesn't conflict with -3 <-2
            * but, this can wait until a more complete set of operators is settled
    * '\~' string operator  
        * since it looks like a string I use it as much as possible for string operations  
        * I'm reluctant to copy Pythons mystr[2:4] indexing for a few reasons  
            * picking up [expr]'s in code can highlight array work  
            * picking up '~' in code can highlight string work  
            * in a general language that seems good for fast grokking of what is being worked on  
            * in narrower domains, e.g. LPeg, math operators can be overloaded to express grammar   
                * the narrow domain and context reduces crosstalk making it effective, unlike in a general language where crosstalk is high  
                * consistent with eliminating crosstalk, I would strive to place math operations outside the grammar  
            * overloading  
                * can be great, but context plays a role in the effectiveness  
    * concat, substr, trim, length, find  
        * concat was done first using '|'   
            * given the discussion above I revised it to '~~'  
        * substr, trim  
            * e.g. "  mystring " ~ 0:0 ~ 5:-1; gives "ring"  
                * a 0 in the zone denotes trim (just one or both ends can be impacted)  
                * partial results appear to flow into subsequent substr operations  
        * length  
            * unary '\~'  
            * e.g. ~ "string" gives 6  
        * find
            * str ~ str   
            * "in" ~ "string" gives 4:5   
            * returning a zone enables further composition  
        * repeat str ^ zone  
            * "*" ^ 5:5 gives *****
            * c ^ (c ~ "     `c`indented") --> *****
    * embedded escape codes  
        * replaced frequently used escapes e.g \t \n  
        * next step would be pattern matching and replacement of escapes in general  
    * strings were great for debugging and ended up being essential for the 8 Queens re-exercise
#  
	PROGRAM ----------------------------- 135 ------------------------------
	001 # composable string concat, trim, substr, length, find, and repeat
	002 # string interpolation using 'backticks`
	003 function main () {
	004   x = "bl" ~~ "    string  " ~ 0:0 ~ 4:-1;
	005   @ x;                      # zone type used for ranges
	006   @ ~x;
	007   @ "in" ~ x; # find gives zone
	008   @ "string" ~ (("in" ~ x) + 1);
	009   c = "*";
	010   @ c ^ (c ~ "     `c`indented")  # print as many c's as indent amt
	011 }
	
	printing: bling
	printing: 5
	printing: 3:4
	printing: in
	printing: ******

12. unless  
    * this was like 'while' and improved my understanding of jumps
    * it showed how easy it is to add new features using previous proven patterns
    * it ended up being a nice touch in the 8 Queens re-exercise

13. multiple dimension arrays
    * these were a great learning experience for nesting indexed expressions
    * this came back to help the implementation of destructuring arrays (see 15 below)
    * syntax a[n1][n2] 
    * bounds checked at runtime
    * 'new' for defining
        * a = new[n1][n2]
        * var a = new[n1][n2]
    * arrays can refer to themselves
    * limitations/tradeoffs
        * arrays are defined non jagged, but in practice can be made jagged
            * the implementation just assigns the attribute 'size' to arrays
               * alternatively 0's could be assigned for initialization
        * however, beware that index bound checking assumes the dimensions given in new statement
        * an improvement might be to enhance the runtime bounds checking to check this
        * or, to detect a break from what was defined and demand that just a single dimension be defined
            * this would eliminate possible confusion
        * an ability to index a single dimension array as though it was multidimensional could be good for efficient matrix processing
    * e.g. a[x,y] would generate a[f(x,y) {return index calculation}]
    * no initializers were defined
        * this is an area rife with possibilities, including ability to initialize from files etc (see Future below)
        * self reference-ability raises a need to handle cycles when e.g. printing them

14. multiple dimension associations (aka associative arrays)
    * similar to arrays, but using keys
    * a = new[] for defining
        * this reuses the overall structure of arrays simplifying use
    * it only makes sense to define the first dimension since additional ones are added to keyed entries created at runtime
        * however, one might imagine static associations defined using elaborate initializers
    * unlike for arrays it occurs to me that I didn't test an association referring to itself
        * but since arrays liked themselves my guess is that they could
    * like for array, initializers is an area rife with possibilities

15. destructuring
    * provided destructuring of both arrays and associations
    * for associations I inserted a sorted keys step from which multiple values would flow into the multiple variables
        * one can imagine all sorts of ways of making association values flow depending on domain
        * used hidden temporary local variables so that swaps could be performed
            * e.g. a[2], a[1] = a;   where a has at least 2 values
            * limitation: a unique table could alternatively be used to hide temp vars to guarantee no name clashes
            * tradeoff: on the other hand, knowing the name pattern enables easier debugging, and duplicate definitions can be detected
    * added ability to destructure into indexed expressions (e.g. above swap operation)
        * var a[2], a[1], x = a;  would swap as well,
        * the local variable x is also created and assigned a[3]
            * nuance: decided to not make indexed expressions local as it doesn't seem to make sense for arrays
        * destructuring works at higher dimensions also
            * e.g. x, y = a["key"];   where a["key"] holds an arary or association
    * destructuring seemed like a step to returning multiple values

16. function default parameters
    * if not used in a call the default values prevail
    * nuance
        * in the same block of a function definition with default parameters
            * var cannot be used for regular parameters since that would be like redefining a var
            * var definitions can however shadow default variables because they are 'hidden'
                * however they do not change the default value (subsequent calls give the same default)

17. forward function declarations
    * nuance
        * my implementation requires parameters of forward declarations to match those given by the definitions
        * this helps with code comprehension and would help with function overloading as well

18. for loop
    * e.g. for i = 1, 10, 2 { body }
        * i is local to body
        * start, end, and step e.g. 1, 10, 2 can be expressions
    * I created a prototypical for loop using var's, an 'and' 'or' logical expression, and while
    * since all the pieces were in the compiler and vm I just needed to compose things in the AST based on the prototype
    * *voila* ... for loop
    * this suggests a design technique of trying to implement flexible primitive features first and then building on top of them
    * safety
        * a step of 0 gives no iterations; using while 1 { body } seems clearer for that use case

19. garbage collection
    * added a nil type to be applied to variables and arrays
    * functionality provided in the vm by Lua
    * limitation: I haven't implemented GC for the interpreter's stack yet
        * would want to benchmark performance effects of that; GC only occasionally?
    
20. just started on adding types
    * started tracking variables, functions, and arrays in the parser
    * this would naturally extend to adding types into the language in the parser
        * for me this raised the question of what "type erasure" means, a concept I've heard only in passing
            * it seems my types would not necessarily flow through to the compiler and vm "erasing" the types
            * would need to investigate pros and cons of erasing or not
    * my first use might be for printing
21. print '@' 
    * added early on
    * I forgot about it until now 
    * **jmpB relative 18 :)**
    * my language vm uses Lua to print arrays and zones
    * an idea would be to associate types with printing methods
    * believe them when they say it is important to keep up with all the homework

22. nested block comments
    * another almost forgotten feature added earlier (jmpB relative 18 again)
    * \# my to-end-of-line comment
    * \#{ my block 
       comment }# for multiline comments, and blocking out code
    * \#{ ... #{ my nested comment }# ... }# for nested multiline comments  
        * created a match time function to track levels of nesting to accommodate nested block comments and precisely report case of missing closings
        * \#{ commentary
            * #{ nested commentary }#  # if the close to the left was missing the error would flow to end of code
        }#  
    * example provided in the test programs

23. standard library
    * per **array.len** function demo'd in 8 Queens test case
    * hooks in parser, compiler, and vm to support standard library style builtin functions

## Future

In this section, I discuss the future of my language, such as its deployability, features, etc.

To complement great languages such as Lua, I'm inclined to keep the general programming aspects simple, but at the same time, say exploit Lua's coroutines to provide automatic pseudo parallel for loops. e.g.

		# coroutine based concurrency with data collection
		for k, v -> array {
			# k , v available locally
			wait { 
				# request expression that takes a long time,
				# must return 1 when completed
			} suspend {
				# final calcs
			} -> expression
		} -> results; # possibly an array of arrays if expression is an array


Other high level constructs might be to create state machines. Otherwise, why not just use Lua?

Another approach would be to wrap systems such as the HPX parallel tasks library. That may entail redoing the VM in C++, using BAPL from within C++, and making my for loops parallelizable in high performance ways.

1. What would be needed to get this project ready for production?  
    * Input facilities. As discussed under 13 above, array initializers may be an area to work on in conjunction with file input.
        * Depending on the domain, I/O could entail interfaces with embedded hardware (including wireless data streams), GUIs, in addition to files
    * Math operators such as sin, cos. 
        * I starting hooking standard library type functionality with array.len
        * once built out, BAPL's capabilities should explode
    * Extend the kind of detailed error reporting I created for variables, keywords, and functions to unbalanced parentheses etc.
        * glimmers of this can be seen in how I reported duplicate function definitions
    * My language can do real world calculations as is. If it can solve 8 Queens, plenty of other problems would fall to it as well. 
        * my entire test suite parsed, compiled, and ran all my test programs in about 0.06 seconds
        * a seprate test to do a mandelbrot plot took longer than I would liked at about 2.5 seconds
        * however, performance seems promising for DSL work
    * For distribution my language could be prepared for hosting within an IDE such as ZeroBrane Studio with a syntax highlighting file, examples, and a "Learn BAPL the Hard Way" Manual (Ideally with Forwards by Roberto and Paul haha)
    * since I've embedded Lua in a C++ optimization engine I created, and C++ can be embedded with Excel, e.g.[ Planatech XLL](http://www.planatechsolutions.com/xllplus/default.htm), hosting within Excel is even conceivable
2. How would I extend this project to do something more? Are there other features I’d like? How would I go about adding them?
    * what general languages don't do out of the box is perform very high level tasks such as what would be provided by a DSL 
        * An interesting next step therefore would be to take the base general capabilities of my language and augment it with DSL abilities. Arrays and associations initializers could take such a form by reading data sources according to a DSL. Similarly, printing could entail something like SAS's very high level proc print. See reference 4 below.
        * A lambda capability might be needed to get to the next level i.e. to enable easy injection of functionality within a high level DSL. 
            * Roberto's discussion of boxed types in Lua's implementation of closures was a revelation. 
            * Lambda's would be a kind of VisiCalc cell within a DSL. 
            * Intentional wormholes between the lambdas is not out of the question. See reference 8 below. 

## Self assessment

In this self assessment of my project: for each criteria described for the final project, I choose a score (1, 2, or 3) and explain my reason for the score in 1-2 sentences.

### Grading Rubic

1. Language Completeness
    * 100% proposed exercises/additions incorporated
    * added features
        * unless
            * didn't think it was needed, but it ended up being a nice touch for 8 queens
        * zone type that acts like a number (overloaded on operators using a metatable)
            * instead of a Boolean type
            * very composable with my string type substr, trim, and find
            * destructurable
        * associated arrays
            * destructurable
            * harmonized with arrays syntax-wise
        * inc dec operations
        * strong syntax error messages (via a captures pattern for precise positioning and context)
            * however, still need to improve error messaging e.g. for missing brackets
            * started toward that with catching repeated function definitions
    * challenge features
        * strings
             * composable set of string specific operations
                * left associativity enables multipe strings operations to compose
                * substr 
                    * e.g. 3:5
                    * exploits computable zone type
                    * avoids conflation with array indexing
                    * negative on end works back from end
                * trim 
                    * e.g. 1:0 trims right side, 0:-1 left, 0:0 both
                    * maximizes use of zone
                * find
                    * returns computable zone type for further composing
                * concat
                    * '~~'
                    * familar string '~' operator; wait for it ... concatenated
                * length
                    * familar '~' operator resued in unary form
                * repeat
                    * enables more composing using zone
                * limitations: need to bullet proof responses to malformed zones
             * interpolation
                * flexibility of expression layout (on next line and multiple next line etc) 
                * for complex expressions retains big picture of desired string structure
                * safe in presence of too few or many expressions
		    * debatable about how to best handle these situations  
			* example of a more composable design (using '~~' for concat and '^' for repetition:
			* s = "{}{} " <-  s  <-  " "^(" " ~ s) ~~ "{}"  <-  name;
			* after filling slots, remaining expressions are used to fill slots in the previously composed string
			* " " ~ s (find space in s) returns a zone
			* ^ (repeat string) would be designed to work with zone
                * flexibility increased by also giving a 'backticks` alternative
                    * use the one that feels comfortable 
        * destructuring
            * works with arrays, associated arrays, and zones
            * also works at higher dimensions
        * for loops
            * for me a higher priority than case-switch
        * started on a type system in the parser
            * became aware of "type erasure" phenomenon
        * interpolation and destructuring are progress toward multiple return values
        * standard library functions
            * required parser, compiler, and runtime changes
            * framework in place to build it out to more functions
            * demo'd in 8 Queens test case
        * personal
            * learning GitHub and creating my first ever repository
            * learning Markdown to create this final report
            * getting the gist of class examples that assumed CS/developer background
    * Score: 3 Combined with doing all the homework and submitting it on time, very important as concepts piled on increasingly, I began adding and experimenting with features early on. This enabled getting some challenging features into my language, and a start on some that should really ramp up what it can do.

1. Code Quality & Report
    * 136 documented/runnable test cases
        * sorted to log all stages of the journey of building a programming language
        * can take 1 or more cases for focusing on specific issues
        * separate options for printing AST, code, and vm
        * many cases commented to help see what is being demonstrated
        * entire suite runs in a blink (.06 secs, .3 secs when including all 8 Queens printed solutions)
    * language came full circle for the course in completing the 8 Queens Ex 2.1
    * this report follows template guidelines (probably a little long winded in places)
    * discussion ranging outside the course material (also see References below)
    * pcalls for Parser, Compiler, VM error handling have been tested
        * traps indicate which component the error originates from
        * currently commented out so any errors are easier to track given stack unwinding messages
    code 
        * 'TODO:'s marked in IDE for a list to help organize work (other IDE's, or a function, could gather them)
            * awareness of weaknesses
        * 'NOTE:'s marked in IDE for a list of things to think about
            * awareness of issues
    * project completed on time per the original schedule
    * when development slows I would break into more files
        * the modules are already there in the one file
        * my priority is to maximize RAD with one file
    * Score: 3 I experienced no frustrations editing my code or making adaptations. No doubt in the future I may regret not adding even more comments in some places, but I also have a generous amount of logging to help.

3. Originality and Scope
    * not vs '~'
        * preserves ~ for string operations 
    * '~'
        * looks like a string so I used it for substr, trim, find, and length
        * resisted copying Python in using [] brackets to index strings
            * used my own original way to experiment composing the operations
            * rationale explained New Features/Changes
        * composable set of operations (concat, substr, trim, length, find)
    * '<-' 
        * arrow like looking operator, visually indicates flow
        * great for interpolation where complex expressions flow into slots, {} in a string
            * literal string for now, but I had used general composable expressions into a string as well
                * trivial to change in the grammar
                * either works, but I need to make a good error message for say, 1 <-2 that somewhat might accidently do, but mean 1 < -2
    * for loop
        * wasn't requested but is a good scope increase
        * afterward the realization came on me that this is more like what I may do when transpiling for a DSL
        * it also provoked thoughts about templating and so forth
    * mathematical relational semantics
    * my own capture pattern approach in place of lhs used by Selene
        * enables precise positioning and wording of syntax error messages
        * still use lhs concept in spirit (e.g. general indexed types are supported, and elegantly implemented, and e.g. support destructuring e.g. for a swap within an array per provided example test cases)
    * modularity
        * I exploited ZeroBrane Studio's outlining, and its red highlighting of non-local functions
            * it gave a constant birds eye view of the overall organization 
            * I could one click to any part of system, edit, and get instant results
            * extremely RAD setup, especially on a 24" pivot, or 30+" monitor (I use a 43" at home)
        * importantly
            * the functions are modular and easily extendable
            * logging is generous and distinguished from printing to expose inputs for extending functionality
            * no name clash issues for me
    * test cases
        * sorted chronologically, grouped, and curated to tell the story that was my journey of building a programming language
        * similarly, all the stages of the journey are captured in my GitHub repository

* Score: 3 Too often I would pause the lecture after getting a gist of where things might be going and then attempt a solution of my own. Sometimes I would get it. Other times it was a disaster. I would not have been free to be as creative without the solid foundation and steady pace laid down by the brilliantly designed lectures. 


4. Self-assessment accuracy
* TBD by mentor

The BAPL course has exceeded my expectations in terms of being a fantastic learning opportunity and in providing unbelievably powerful and accessible tools for creating a programming language. This, in a stimulating well orchestrated environment along with superb instruction and mentoring. Together with others pursuing similar interests. The course has also given me a much greater appreciation for programming languages in general. In a word, the course was a blast.

In particular the course increased my appreciation for [Lua](http://www.lua.org/home.html). What a great contribution Roberto has made in giving it along with LPeg to the developer community. [ZeroBrane Studio](https://studio.zerobrane.com/) helped me, a lay developer at best, not get bogged down managing my code. I don't see how I would have kept pace without it.

## References

Since I provided a picture of my computer books on Discord BAPL "off-topic" thread it may be more useful here to highlight some unseen material feeding into my experience of BAPL.

<img src="https://cdn.discordapp.com/attachments/1141752676586111036/1158066680946233455/image.png?ex=653e7da4&is=652c08a4&hm=43f63cb21c919feef5cfdbe04029a86668ffe10c570f649e9055f0df13eaefdc&" width="400" height="600">

I found the course to be largely self-contained, although as a lay developer I had only cursory experience with regular expressions, Python, JavaScript, GitHub, and Markdown (that I am learning as I type this up). Fortunately, as side gigs, I had acquired some experience creating recursive procedures in the process of creating a hydro generation plant optimization engine in Mathematica. Then recreating it in C++, and embedding Lua into it, gave me further experience with Lua. I use Lua to access the engine and create more optimization solutions. Since I have never been a software developer in any official capacity my side gigs were helpful background for coming into the BAPL course.

I have a few references related to language creation in my library, but never got far with them. Now, after taking this course, I can take them off my shelves and have fun with them!

The following references highlight, in roughly chronological order, influences on my approach to building a programming language.

1. COMAL

    * [COMAL Guide](https://petlibrary.tripod.com/ccbg3.htm)
    * when BASIC was all the rage, I gravitated to COMAL
    * even given BASIC, I was drawn to creating DSLs with it (although mine were terrible hacks)
    * with Lua/LPeg it seems it would be a breeze to bring old flames like COMAL back to life for a fling

2. PL/1
    * [IBM PL/1 Docs](https://www.ibm.com/docs/en/zos-basic-skills?topic=zos-pli)
    * despite the nightmare of using JCL (Job Control Language) to submit code to the mainframe, PL/1 was a dream  
    * I regret throwing out my textbook on it (used for a night class I took)  
    * PL/1 was used extensively in BC Hydro, before it dropped its license, due to high cost; 
    * that left us with Fortran, which I still think is hard to beat for numerical work  
    * a Resource Planning Electrical Engineer created a beautiful planning model using double linked lists and that performed convolution to estimate future requirements  
        * as an EIT (Engineer in Training) I helped him convert it to Fortan, but sadly missed PL/1's double linked lists
        * some major BC Hydro PL/1 programs are still being run on an old PC running PL/1 e.g. to simulate watersheds  
3. ABC
 
    * [ABC Handbook](https://homepages.cwi.nl/~steven/abc/programmers/handbook.html)
    * another book I regret throwing out
    * should have kept all my computer books (even the one on REBOL)
    * ABC seemed like a gem of a language with innovative design decisions

4. The Complete Guide to the SAS Macro Language
    
    * This can be seen on my shelf, but not my creation of "UPRINT, A New Kind of SAS Pseudo Procedure". UPRINT was for the SAS system and as a side gig I built (hacked) it using the SAS Macro Language. UPRINT was my first successful attempt at a DSL. My paper on it was accepted by the SAS Institute. I noticed that over the years SAS Institute extended the idea to greatly enhance their built-in PRINT procedure.
    * I used UPRINT at BC Hydro for a decade as a DSL to produce complex reports. My last use of it was to manage KPI's for the teams I was managing. Peer managers came to me to perform the magic needed to get a handle on their KPIs and to report them to higher management. Then BC Hydro ended their SAS subscription due to its very high cost. Thankfully, the obsession with KPIs faded at the same time.
    * SAS seems geared more for national and multi-national size companies. Most people probably use R or Python, but SAS could efficiently handle out of memory enormously large datasets.
    * Until this course however, I never had accessible tools to create DSLs with the rigor I desired. 
    * Except for in SAS, I haven't seen the kinds of things UPRINT could do, I may re-implement UPRINT using the knowledge and tools gained here.
        * [PROC PRINT](https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/proc/p10qiuo2yicr4qn17rav8kptnjpu.htm)
    * And here is a pleasant surprise ...
        * [LUA as SAS Macro Language](https://support.sas.com/resources/papers/proceedings17/SAS0212-2017.pdf)  
        * What a confirmation of Lua. SAS now uses Lua as their macro language. How amazing that today, if I was inventing UPRINT, as a side gig again, it would be in Lua!
    * Another system that uses Lua as its macro language, or meta language as they say, is [Terra]( https://terralang.org/). I would like to further explore that approach.
5. Object Logo
    * [Digitool, Inc **Object Logo**](https://web.archive.org/web/20080430083030/http://www.digitool.com/ol-specs.html)
    * With its Lisp underpinnings, **robotics** interfaces, a BAPL project may have potential within something like Object Logo. It would be interesting to have a Selene that did graphics or robotics. For some (see 10 below) that may make a big difference. 
    * For my Electrical Engineering Degree Thesis, I used Object Logo to create a graphical CAD system for a power systems load flow analysis program I created using Mathematica 1.0. It ran on a Mac SE connected to an enormous external monochrome high resolution monitor.
    * systems like Object Logo make me want to be able to go back to the future
6. Databases
     * [4th Dimension](https://us.4d.com/)
     * [FoxPro](https://learn.microsoft.com/en-us/previous-versions/visualstudio/foxpro/mt490117(v=msdn.10))
     * [Microsoft Business Intelligence DAX](https://learn.microsoft.com/en-us/dax/)
    * But honestly, with text files and Lua, and SQL like functions built in Lua, I can manage a lot of the data I use and generate.
    * SSD drives were a game changer making text files seem more like in-memory data.
    * Dependance on data administrators has its place despite how expensive it is, but for a lot of applications it is an unnecessary price to pay.
7. Excel/VBA
    * Alas, Excel serves for many of my creations. It facilitates wider distribution of them.
    * I shell out to my C++ optimization engine to allow Excel to be its front end. 
    * The whole affair is contained in a folder that can be dragged and dropped for deployment.
        * It both amazes and frustrates our IT department who major in LOB C# applications. 
        * A variety of contributors helps move things forward.
    * I'm delighted to see Excel get [lambdas](https://visualstudiomagazine.com/articles/2021/01/27/excel-lambda.aspx) in its formulae language and that there are signs that [Python](https://www.anaconda.com/blog/announcing-python-in-excel-next-level-data-analysis-for-all) might become an alternative to VBA one day. 
8. Mathematica 
    * [Wolfram Language](https://reference.wolfram.com/language/?source=footer) as its known more recently
    * the online documentation libary for MMA, if printed, would require another bookcase of its own
    * has what looks like a grammar of grammars, including [GrammarRules](https://reference.wolfram.com/language/guide/ProgrammableLinguisticInterface.html)  
    	* it would be interesting to get your take on it
    * MMA illustrates some very general ways of doing things
        * e.g. Cases selects data using patterns
    * I steal from MMA when creating functions in Lua to handle data. 
        * e.g. my punchy 'take' function in my base file for BAPL is inspired by MMA
    * MMA even has [wormholes](https://www.wolfram.com/broadcast/video.php?c=385&disp=list&v=1336)
    * A MMA motto is "everything is a symbolic expression" so no language is higher level than it. This is a segway to C++ whose mottos include "except  Assembly, no language is lower level than it". 
9. C++
    * It seems popular to decry C++'s complexity, but I look at C++ through the lens of doing renovations. I have done a lot of renovations over the years on my houses and have a bookcase of electrical, plumbing, HVAC, framing, cabinet making, etc books and journals. Ditto on car hotrodding, and mountain bike rebuilding fronts. Things can be very messy under the paint, floor, or hood. But the end results can be beautiful and easy to live with. People are the same. Messy guts under relatively beautiful skin. Behind their smooth speech and body language lies incredibly messy gray matter.
    * C++ allows one to build beautiful abstractions. So what if things are very complicated and messy under the skin. I would encourage enjoying the skin of C++ unless one specializes in providing a particular abstraction, in which case by all means get bloodied. Good abstractions can take many years to mature, so I just wait till they are ready for prime time. For example, I have yet to convert to using the C++ range library.
    * Bjarne's "A Tour of C++" seems like a good example of focusing on the surface of C++ and enjoying it. At the same time I appreciate that we can smash walls down so to speak, get down and dirty, and maybe after years of work, present a beautiful new very high performing abstraction.
    * Nevertheless, in the past I have also enjoyed purchasing a new house, a factory hotrodded car, an already maxed mtb, and I look forward to investing in [Cpp2]( https://www.reddit.com/r/cpp/comments/171o3q9/cooperative_c_evolution_toward_a_typescript_for_c/)
    * Cpp2 is being pitched as a "TypeScript", a meme for accepting 100% backward compatibility with an established language. However, it enables going forward with major new features (and optionally but easily leaving undesirable features behind). 
    * Is there a place for a "Typescript" of Lua?      
        * Could it incorporate the best of what global and default declaration and access should mean?
        * Could it have a purer Boolean situation?
        * Could it seamlessly incorporate  the best of Pallene (e.g. JIT compilation) and remain one language going forward?
        * Could it remain as simple and flexible as ever, and become more powerful than ever?
    * Mottos?
        * C++ no language should be lower level than C++ (except Assembly)
        * MMA no language should be higher level than MMA
        * Lua no language flexibility/complexity ratio should exceed Lua
10. MBTI
    * I completed a fulltime one weeklong live classroom course and got certified to administer the [MBTI personality type indicator test](https://www.themyersbriggs.com/en-US/Get-Certified/MBTI-Certification?gclid=Cj0KCQjwhL6pBhDjARIsAGx8D5_djhYP-Ns-yJKd0_J60wcZdWNl4CtSBY0LXx8RXsZcREVmiq5OBksaAkDzEALw_wcB)   
 **There are type systems for people too!** 
    * I enjoy guessing people's personality type before they take the test. Often, especially given some time interacting with them, I guess right. My kids are into Enneagram as an alternative. I prefer the more professional five personality traits, but they all bring something to the table.
    * What has this got to do with building a programming language? Well, surprising differences of style and opinion can originate from people having different personality types. It seems helpful to keep in mind the different ways of approaching problems and different solutions that some people may prefer and others not. This means that it can be a good thing to provide multiple ways to solve a problem. Lua succeeds with this by providing mechanisms that can be used in different ways. Similarly, ZeroBrane Studio provides a variety of ways to interact with it (mouse, keyboard, specification files, compilation, etc)
    * The personality type differences come into play in what one might consider to be a good programming language or DSL, especially if one aims to make it a general one. We can't be all things to everyone, or "boil the ocean" as former Intel CEO Andy Groves put it, but working within a broad narrative seems like a noble goal.

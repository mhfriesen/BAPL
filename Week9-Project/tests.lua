local base = require"base"
local factorial = base.factorial

main_cases = {
137,[[
function main() {
  xmin = -2.0;
  xmax = 1.0;
  ymin = -1.5;
  ymax = 1.5;
  width = 80.0;
  height = 40.0;
  threshhold = 1000;
  deltax = (xmax - xmin) / width;
  deltay = (ymax - ymin) / height;
  _x = 0.0;
  _y = 0.0;
  tmp = 0.0;
  n = 0;
  inside = 0;
  for y = ymax, ymin, -deltay {
    x = xmin;
    for x = xmin, xmax, deltax {
      _x = 0.0;
      _y = 0.0;
      n = threshhold;
      inside = 1;
      while 0 < n {
        tmp = _x*_x - _y*_y + x;
        _y = 2.0*_x*_y + y;
        _x = tmp;
        n = n - 1;
        if ((_x*_x + _y*_y) > 4.0) {
          inside = 0;
          n = 0;
        };
      };
      @ inside and "#" or " ";
    };
    @ "\n"
  };
}]]
,
136,[[
#{
  where BAPL began with chapter 1-2 of PILua book Ex 2.1 ...
  enhanced with 'forward function declaration', 'unless', 'string interpolation', 
  and a royal athem
}#

function addqueen(rows_col, cur_row);

function main () {
  var N = 8;
  print_first_soln_only = 1;
  isa_soln_printed = 0; # to stop after printing first solution
  var qs = new[N];
  addqueen(qs,1);
  return "God Save the Queen."
}

function isplaceok (rows_col, cur_row, col) {
  for row = 1, cur_row - 1 { # for each queen already placed
    var previous_queen_col = rows_col[row];
    if (previous_queen_col == col or                    # same column
       (previous_queen_col == col - cur_row + row) or   # same diagonal to left
       (previous_queen_col == col + cur_row - row) ) {  # same diagonal to right
        return 0
    }
  };
  return 1
}

function printsolution (rows_col) {
  var s = "\n";
  for row = 1, array.len(rows_col) {
    for col = 1, array.len(rows_col) {
      s = "{}{} "  <- s  <- (rows_col[row] == col and "X"  or  "_")
    };
    s = s | "\n"  
  };
  @ s
}

function addqueen(rows_col, cur_row) {
  if cur_row > array.len(rows_col) {
    printsolution(rows_col);
    isa_soln_printed = 1
  } else {
    for col = 1, array.len(rows_col) {
      if isplaceok(rows_col, cur_row, col) {
        rows_col[cur_row] = col;
        unless print_first_soln_only and isa_soln_printed {
          addqueen(rows_col, cur_row + 1)
        }
      }
    }
  }
}]]
,
135,[[
# composable string concat, trim, substr, length, and find
function main () {
  x = "bl" | "    string  " ~ 0:0 ~ 4:-1;
  @ x;                      # zone type used for ranges
  @ ~x;
  @ "in" ~ x; # find gives zone
  @ "string" ~ (("in" ~ x) + 1);
}]]
,
134,[[
# the parser lowers for loops to while loops (i.e. no compiler or vm changes required)
function main () {
  i = 42;
  for i = 1, 3 {      # positive direction
    @ i               # i is local
  };
  return i
}]]
,
133,[[
function main () {
  var i = 42;
  for i = 3, 1, -1 {  # negative direction
    @ i
  };
  return i
}]]
,
132,[[
function main () {
  var x = 2;
  var a = new[5];
  for i = 1 + 2!, 7 - x { # expressions
    a[i] = i:(i^2);
    if i == 3 {return "returning out of a for loop"}
  };
  return a
}]]
,
131,[[
# swap using array destructuring
function main () {
  a = new[2][3];
  a[1][1] = 10;
  a[1][2] = 20;
  @ a;
  a[1][2] , a[1][1] = a[1];  # swap in 2nd dim  
  return a
}]]
,
130,[[
# swap using array destructuring - nuances
function main () {
  a = new[2][3];
  a[1][1] = 10;
  a[1][2] = 20;
  a[1][3] = 42;
  @ "{}\n" <- a;
  var a[1][2] , a[1][1], x = a[1];  # swap and var  
  @ x; 
  # x is a local
  # arrays remain whatever they were
  return a
}]]
,
129,[[
function main () {
  outer = 99;
  {
    var a = new[];        # associative array
    for i = 1, 5 {
      a["k"|i] = i:(i^2)
    };
    x, outer, z = a;      # associative destructuring
    @ outer;
  };
  return outer
}]]
,
128,[[
function main () {
  outer = 99;
  {
    var a = new[];        # associative array
    for i = 1, 5 {
      var b = new[];
      for j = 1, 3 {
        a["k"|i] = b;
        a["k"|i]["j"|j] = i:j
      };
    };
    @ a["k3"]["j2"];
    var x, outer, z = a["k3"]; # local associative destructuring
    @ outer;
  };
  return outer
}]]
,
127,[[
# prototypical for loops using while
# 'and' is higher priority than 'or' so no parentheses needed
function main () {
    var i = 6;
    var to = 0;
    var step = -2;
    while 0<step and i<=to or step<0 and to<=i { 
      @ i;
      i = i + step
  }
}]]
,
126,[[
function main () {
  {
    var i = 1;
    var to = 6;
    var step = 2;
    while 0<step and i<=to or step<0 and to<=i { 
      {@ i};
      i = i + step
    } 
  }
}]]
,
125,[[
function main () {
  var i = -99;
  for i = 1, 6, 2 {
    @ i
  };
  return i
}]]
,
124,[[
function main () {
  var i = -99;
  for i = 6, 1, -2 {
    @ i
  };
  return i
}]]
,
123,[[
function main () {
  var n = 0;
  var i = 4; 
  while i {
    var j = 4;
    while j {
      j = j - 1;
      n = n + 1
    };
    i = i - 1
  };

  return n
}]]
,
122,[[
function main () {
  var N = 4;
  var a = new[N][N];
  var i = 1; 
  var n = 0; 
  while i <= N {
    var j = 1;
    while j <= N {
      a[i][j] = i * 10 + j;
      @ "a[{}][{}] = {}" <- i  <- j  <- a[i][j];
      j = j + 1;
      n = n + 1;
    };
    i = i + 1;
  };
  return n;
}]]
,
121,[[
# default parameters
function fact(n, i = 35, c = 5 + 2, e = 0) {
  @ "ARGS: n={} i={} c={} e={}"  <- n  <- i  <- c <- e  ;
  if n {
    var e = i + c + e; # locals do not override unseen defaults
    @ "LOCS: n={} i={} c={} e={} NB: param e not modified, only shadowed"  <- n  <- i  <- c  <- e  ;
    return n * fact(n-1, 30, 12)
  } else {
    return 1
  }
}

function main() {
    return fact(6)
}]]
,
120,[[
# so, now let's try some indirect recursion
function foo(x);

function bar(x) {
  if 42 < x {
    return foo(x)
  } else {
    return x
  }
}

function foo(x) {
  x = x - 1;
  return bar(x)
}

function main ( ) {
  return bar(45)
}]]
,
119,[[
function main() {
  s = 10;
  a = new[s][2];
  i = 1;
  @ "Let's have fun for a while!";
  while i <= s {
    a[i][1] = i!;
    a[i][2] = i^2;
    @ i | "! = " | a[i][1] | "    " | i | "^2 = " | a[i][2];
    i = i + 1
  };
  return "Yay"
}]]
,
118,[[
function main() {
  if 1 or 0 {
    @ "it's true"
  };
  unless not 1 or 0 {
    @ "unless it isn't"
  };
  return 7
}]]
,
117,[[
#{
  Impact of a single new line in function tozone:
  t["size"] = 2 -- this attribute causes zone type to inherit
                -- all the functionality of array type :)
}#
function main() {
  z = 3:9 + 1:-7;  
  @ z;
  @ "1 ---> {} {}"  <-  z[1]  <- z[2]; 
  @ "2 ---> {} {}"  <-  z; # but zone still has it's own tostring behavior
  a = new[2];
  a[1] = 4; a[2] = 2;
  @ "2.1 ---> {} {}"  <-  a; 
  x, y = z;
  @ "3 ---> {} {}"  <-  x <-  y; 
}]]
,
116,[[
# associative arrays
function main ( ) {
  a = new[];
  b = new[];
  a["foo"] = b;
  a["foo"]["bar"] = "is";
  @ "it {} what it {}"  <-  a["foo"]["bar"];
  
  a = new[];
  a["y"] = 2;
  a["x"] = 4;
  m, n = a;  # destructuring
  @ "{}{} since keys are sorted for interpolation"  <-  m  <-  n;
}]]
,
115,[[
# things are too structured
function main() {
  a = new[3];
  a[1] = 5;
  a[2] = 6;
  a[3] = 7;
  
  x, y, z = a;   # so, we will now destructure them
  @ "{}, {}, and {}" <- x <- y <- z;
  
  @ "{}'s are literally polite"  <-  "{}";
  @ "strings { } can say \"no thankyou\""  <-  "surplus"  <- "exprs";
  @ "but they {} to be padded on the back {} this"  <-  "like";
  
  return x:y + z   # zone type still working
}]]
,
114,[[
# six tests for short cct and and or
function main() {
  @ 0 and 4 < 3;
  return 7
}]]
,
113,[[
function main() {
  @ 1 and 4 < 3;
  return 7
}]]
,
112,[[
function main() {
  @ 1 and 2 < 3;
  return 7
}]]
,
111,[[
function main() {
  @ 0 or 4 < 3;
  return 7
}]]
,
110,[[
function main() {
  @ 1 or 4 < 3;
  return 7
}]]
,
109,[[
function main() {
  @ 1 or 2 < 3;
  return 7
}]]
,
108,[[
function foo() {return 99}
function main() {return foo()}]]
,
107,[[
function foo();
function foo() {return 99}
function foo() {return 42}
function main() {return foo()}]]
,
106,[[
function foo() {return 42}
function main() {return fo()}]]
,
105,[[
function foo();
function main() {return foo()}]]
,
104,[[
function inc() {return 42}
function main() {return inc()}]]
,
103,[[
function foo() {return 42}
function main() {return foo}]]
,
102,[[
function main() {
  if 0 {
    @ 99
  } else {
    @ 999
  };
  return 7
}]]
,
101,[[
function main() {
  if 1 {
    x = 1
  } else {
    x = 2
  };
  return 7
}]]
,
100,[[
function main() {
  if 2 < 1 {
    return 5
  } elseif 0 {
    return 7
  } elseif 0 {
    return 9
  } else {
    if 0 {return 99} elseif not 1 < 9 < 3 {return 999}
  };
  return "never this"
}
]]
,
99,[[
# demo how inc does not modify variable like ++ in C++
function main () {
  x = 1;
  return inc x + x;
}]]
,
}

-- pre functions tests now surrounded in function main() {   ... test }
prefunction_cases = {
98,[[
x=1; y="s";
@ ("Here are {} interpolation{} \n with {} expressions"  <-  12 / 3! + x  <-  y <- "v" | "arious") | ", and a bit more." 
]]
,
97,[[
@ "str"]]
,
96,[[
a = new[3];
a[1] = a; #--- arrays that like themselves
@ a]]
,
95,[[
a = new[3];
b = new[2];
a[2] = b; #--- arrays that like other arrays
@ a]]
,
94,[[
a = new[4][3];
a[2][2] = 5;
a = nil;  #--- garbage collection
a[2][2] = 6;
@ a]]
,
93,[[
x = 1;
@ x;
x = nil;
@ x]]
,
92,[[
a = new[4][3];
b[3][5] = 9999;
@ b]]
,
91,[[
a = new[4][3];
a[3][2] = 9999;
@ a[3][2];
a[2][3] = 99;
return a[2][3] ]]
,
90,[[
a = new[4][3];
a[3][3] = 9;
@ "oooo... string type" | " so we can say we got to line " | (2 + 1);
a[3][5] = 9999;
@ "wish we got this far"]]
,
89,[[@ "mind those runtime errors" | 2 + 1]]
,
88,[[
a = new[4][3];
@ "got here";
@ a[3][5];
@ "but not here, so go figure"]]
,
87,[[
s= 3;
a = new[(s+1)/2+1][s-1];
a[2][1] = 999999;
return a]]
,
86,[[
a = new[10];
a[9] = 4;
a[10] = a[9];
@ a[10];
@ a;
return a]]
,
85,[[
x=1+inc[3]; return x]]
,
84,[[
x=1+inc 4; return x]]
,
83,[[
return a]]
,
82,[[
return "abc" ]]
,
81,[[
return not 1 < 8 < 4]]
,
80,[[
return not 1 < 8 < 4!]]
,
79,[[
return 2-2 and 5-3 or 4+1]]
, 
78,[[
return not 1 or 3 and 5]]
,
77,[[
return 1! and 0 or 7]]
,
76,[[
if 0 {
  return 5
} elseif 0 {
  return 7
} elseif 0 {
  return 9
} else {
  return 999
}]]
,
75,[[
if 2 < 1 {
  return 5
} elseif 0 {
  return 7
} elseif 0 {
  return 9
} else {
  if 0 {return 99} elseif not 3 < 2 {return 999}
}]]
,
74,[[
if 2 < 1 {
  return 5
} elseif 0 {
  return 7
} elseif 0 {
  return 9
} else {
  if 0 {return 99} elseif not (1 < 9 < 3) {return 999}
}]]
,
73,[[
if 2 < 1 {
  return 5
} elseif 0 {
  return 7
} elseif 0 {
  return 9
} else {
  if 0 {return 99} elseif not 1 < 9 < 3 {return 999}
}]]
,
72,[[
if 0 {
  return 5
} elseif 1 < 3 < 5 {
  return 7
} elseif 0 {
  return 99
}             # no else]]
,
71,[[
if 0 {
  return 5
} elseif 0 {
    return 7
} elseif 0 {
    return 99
}             # no else]]
,
70,[[
if 0 {
  return 5
} else {
  if 0 {
    return 7
  } else {
    return 99
  }
}]]
,
69,[[
x = 1 < 5 < 4;  # oooo... mathematical relations
y = 1 < 4 <= 4;
return x:y  # zone type used for multiple return values]]
,
68,[[
x = 5;
return 1 < x < 4 < 7]]
,
67,[[
return x = 1 < 5 < 4]]
,
66,[[
{;; }; 
{ ;         # empty statements galore!!
  x = 3;;;;
  y; = 1:2; # but, this is not an empty statement
}; 
return x+y]]
,
65,[[
x = - inc -4; return x]]
,
64,[[
#{my block comment #{ with a nested block comment # just a comment too
  here }# and rest of comment } # return 1;]]
,
63,[[
return y]]
,
62,[[
de =dec dec 1; return de #----- unary ops]]
,
61,[[
de =-dec dec 1; return de]]
,
60,[[
de =dec -dec 1; return de]]
,
59,[[
de =dec - dec 1; return de]]
,
58,[[
de =dec dec 1; return = de]]
,
57,[[
# the next three lines are fine
x = 1; #{ this, but not the # embedded comments
is a block comment spanning #{nesting allowed of course} #, multiple lines }#
@ (x -3<2);   
@ ((not 3<-1);  # but this line has a syntax error
return y
]]
,
56,[[
x=1:3; 
y=dec 2; 
z= inc 4; 
@ (x+7); # should print 8:10
@ 2<3; 
@ not (2<3); 
return -x + y+z
]]
,
55,[[
x = y!; return x]]
,
}

-- pre return function now require wrapping with return as well as function main
primodial_cases = {
   "11 %4",tostring(math.fmod(11,4)),
   "2^3 * 0x02","16",
   "2.^(3 * 4)",tostring(2.^(3*4)),
   "(0x02+5)","7",
   "3!","6",
   "-3",
   "-(-3)! ",
   "-(-3)! +2",
   "-(-3)! ^2",
   "-(-3)!! ^2",tostring(factorial(factorial(3))^2),
   "10<20",
   "10>20",
   "10==10",
   "10<=20",
   "10>=20",
   "10~=10",
   "10~=11",
   "10:20 + 5",
   "10:(20 + 5)",
   "-10:20 + 5",
   "-10:20 <-5:30",
   "-10:20 <=-5:30",
   "-10:20>=-5:30",
   "10:20 ==10:20",
   "10:20 ==10:5",
   "2 - 5",
   "2-5",
   "2 - -5",
   "-1:(2^2) - (3!):5",
   "2:3-2:3",
   "2:3- -2:3",
   "2:3-(2-4):3",
   "2:3|5:8",
   "2| 15",
   "-0X02| 15",
   "-.1:0.2  - -0.2:.3",
   "-(-.1:0.2  - -0.2:.3)",
   "-0X02:0xff + 2e10-2e1",
   "0x02 + 0X03",
   "0x02c + 0X03", 
   "--1",
   "-(-1)", 
   "1- -3",
   "5 - - -1",
   "5-1",
   "-5-1",
   "2^3^2",
}

-- group number, case
local grouped_main_cases = {}
for i = 1, #main_cases, 2 do
  table.insert(grouped_main_cases, {main_cases[i], main_cases[i+1]})
end

local function indent(s)
  local s2 = "  "
  for i = 1, #s do
    local sub = string.sub(s,i,i)
    if string.byte(sub) == 10 then
      sub = sub .. "  "
    end
    s2 = s2 .. sub
  end
  return s2
end

local grouped_prefunction_cases = {}
for i = 1, #prefunction_cases - 1, 2 do
  local case = prefunction_cases[i+1]
  local indented = indent(case)
  table.insert(grouped_prefunction_cases, {prefunction_cases[i],"function main() {\n" .. indented .. "\n}"})
end


local grouped_primordial_cases = {}
local n = 1
for i=#primodial_cases, 1, -1 do
  primodial_cases[i] = "  return ".. primodial_cases[i]
  primodial_cases[i] = "function main() {\n  " .. primodial_cases[i] .. "\n}"
  table.insert(grouped_primordial_cases, {i, primodial_cases[i]})
  n = n + 1
end



local M = {}

M.main_cases = grouped_main_cases
M.prefunction_cases = grouped_prefunction_cases
M.primordial_cases = grouped_primordial_cases

return M

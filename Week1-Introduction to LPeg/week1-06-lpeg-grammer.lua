local lpeg = require"lpeg"
local lfs = require"lfs" --luafilesystem
dofile(lfs.currentdir().."\\pt.lua") --table to string

-- test LPEG pattern on a table of strings e.g. test("arithmetic", p, {"1+1", "   1 +2"})
function test(name, pattern, tests)
  print("\n"..name)
  local str, chk
  for i,test in ipairs(tests) do
    if type(test)=="table" then -- includes a check
      str, chk = test[1], test[2]
    else
      str = test
      chk = nil
    end
    io.write(str,"--->")
    local results = {pattern:match(str)}
    if #results == 0 then
        io.write("nil")
        if chk then
          local isgood = tostring(results[#results]) == tostring(chk)
          io.write("===>",tostring(chk)," ", isgood and "GREAT!" or "OH NO!")
        end
    else
      for _,r in ipairs(results) do
        io.write(tostring(r)," ")
      end
      if chk then 
        local isgood = tostring(results[#results]) == tostring(chk)
        io.write("===>",tostring(chk)," ", isgood and "GREAT!" or "OH NO!")
      end
    end
    io.write("\n")
  end
end

print("\n" .."BASIC LPEG PARSING")
underscore = lpeg.P("_")
alpha = lpeg.R("az","AZ")
numeric = lpeg.R("09")
alpha_numeric = alpha + numeric
id = (alpha + underscore) * (alpha_numeric + underscore)^0
notSpace = -(1-lpeg.P(" ") )
identifier = id * notSpace
object = id * "." * id * notSpace 

identifiers = {
  {"_",2},
  {"0","nil"},
  {"_0",3},
  {"_A",3},
  {"abc",4},
  {"a_b ",4},
  {"myID_",6},
  {"_MYid_",7},
  {"M",2},
  {"a.","nil"},
  {"a .", 2},
}
objects = {"abc","abc.",".a","_._", "_.0", "A.B", "0.a", "a.b aa"}

test("match indentifiers", identifier, identifiers)
test("match objects", object, objects)


print("\n" .."NON-EMPTY LIST OF NUMBERS ALLOWING SPACES")
space = lpeg.S(" \t\n")^0
operator = lpeg.P("+") * space
numeral = lpeg.R("09")^1
period = lpeg.P(".")
number =  (numeral * period * numeral +
          numeral * period  + 
          period * numeral  + 
          numeral  
          ) * space
        
p = space * numeral * (operator * numeral)^0

case = "\n" .."summation pattern match a "
summations = {"1+1", " 1+1", " 1+1 ", "1 +1", "1+ 1","1 + 1","abc","","1+2+3","1+2 +3","\t1\t+\t2\n","1.+2"}
p = space * numeral * space * (operator * numeral * space)^0
test(case,p,summations)

case= "\n" .. "per above but with decimal point"
summations = {"1+10", " 10+1", " 10+1 ", "1 +10", "10+ 1","10 + 1","abc10","","1+20+3","1+2 +03","\t1\t+\t2\n","1.+2",".1+3.","1.1 + 3.2"}
p = space * number * (operator * number)^0
test(case,p,summations)

case = "\n" .. "per above but with +-*/ operators"
operator = lpeg.S("+-*/") * space
p = space * number * (operator * number)^0
test(case,p,{
    "12+13+25",
    "12 + 13+  25 ",
    " 10*1",
    " 10/1 ",
    "1. -.10",
    "10+ 11 a",
    "10 + 12",
    "abc10",
    "",
    "11+20+99",
    "100+200 +03",
    "\t1\t+\t2\n",
    "1.+2",".1+3."
    ,"1.1 + 3.2",
})

case = "\n" .. "per above capturing numbers and positions of operators"
capture = lpeg.C
p = space * capture(number) * (lpeg.Cp(0) * operator * capture(number))^1
test(case,p,summations)

case = "\n" .. "per above when the whole subject"
p = space * capture(number) * (lpeg.Cp(0) * operator * capture(number))^1 * -1
test(case,p,summations)


print("\n" .."ACCUMULATE NUMBERS")

function add(acc, newvalue) return acc + newvalue end

case = "\n" .. "basic numbers"
numberv = lpeg.R("09")^1 / tonumber
p = space * numberv * ("+" * numberv)^1 / add * -1
test(case,p,{"12+13"})

case = "\n" .. "numbers can have signs and are always captured and tonumber'd"
sign = lpeg.S("+-")^-1
number =  sign * (
          numeral * period * numeral +
          numeral * period  + 
          period * numeral  + 
          numeral  
         )  / tonumber * space  -- / func makes it a capture also
        
p = space * number * (operator * number)^0 / add * -1
test(case,p,{" 12+ -13", " -5.2 - +1 ", " -5.2 - +-1 "})


function fold(t)
  local acc = t[1]
  for i=2, #t do
    acc = acc + t[i]
  end
  return acc
end

case = "\n" .. "fold + or - operators with captures in table"
Ct = lpeg.Ct
p = space * Ct(number * (operator * number)^0) / fold * -1
test(case,p,{" 12+ -13 - 2", " -5.2 - +1 - .1"})

  
function foldcaptures1(t, f) -- just + or - operators
  local acc = t[1]
  for i=3, #t, 2 do
    acc = acc + (t[i-1]=="+" and t[i] or -t[i])
  end
  return acc
end

function fold3(t, f)
  local acc = t[1]
  for i=3,#t,2 do
    acc = f(acc,t[i-1],t[i])
  end
  return acc
end

opfuncs = {
  ["+"] = function(a,b) return a+b end,
  ["-"] = function(a,b) return a-b end,
  ["*"] = function(a,b) return a*b end,
  ["/"] = function(a,b) return a/b end,
  ["%"] = function(a,b) return math.fmod(a,b) end,
}

function foldcaptures(t) -- use above table of ops
  return fold3(t, function(acc, op, n) return opfuncs[op](acc, n) end)
end
    

operator = lpeg.C(lpeg.S("+-*/")) * space -- also capture operators

case = "\n" .. "acct for minus operator"

p = space * Ct(number * (operator * number)^0) / foldcaptures1 * -1
test(case,p,{" 12+ -13 - 2", " -5.2 + 1 - .1"})

case = "\n" .. "generalize folding"

p = space * Ct(number * (operator * number)^0) / foldcaptures * -1
test(case,p,{" 12+ -13 - 2", " -5.2 + 1 - .1"})

print("\n" .."LPEG BASIC PRECEDENCE")

opA = lpeg.C(lpeg.S("+-")) * space 
opM = lpeg.C(lpeg.S("*/%")) * space 
opE = lpeg.P("^") * space
exp = space * lpeg.Ct(number * opE * number) / function(t) return t[1]^t[2] end + number
term = space * Ct(exp * (opM * exp)^0) / foldcaptures
sum = space * Ct(term * (opA * term)^0) / foldcaptures * -1

print("exp", exp:match"10^3")
case = "\n" .. "mult and div with precedence"
test(case,sum,{" 12+ -13 - 2", " -5.2 + 1 - .1", "2 + 14 * 2", "2 + 14 * 2/2 -30","3%2 * 5 + 10^2 *3"})


print("\n" .."LPEG GRAMMER")

case ="\n".."basic precedence"

exp = lpeg.V"exp"
primary = lpeg.V"primary"
term = lpeg.V"term"
expression = lpeg.V"expression"

g = lpeg.P{"expression",
  exp = space * lpeg.Ct(number * opE * number) / function(t) return t[1]^t[2] end + number,
  term = space * Ct(exp * (opM * exp)^0) / foldcaptures,
  expression = space * Ct(term * (opA * term)^0) / foldcaptures 
}
g = g * -1

test(case, g, {
  "1+1 *10 ^2",
})

case = "\n".."with parathethises"

OP = lpeg.P"(" * space
CP = lpeg.P")" * space

g = lpeg.P{"expression",
  exp = space * lpeg.Ct(number * opE * number) / function(t) return t[1]^t[2] end + number,
  primary = exp + OP * expression * CP,
  term = space * Ct(primary * (opM * primary)^0) / foldcaptures,
  expression = space * Ct(term * (opA * term)^0) / foldcaptures 
}
g = g * -1

test(case, g, {
  "1+1 *10 ^2",
  "(1+ 1) *10 ^2",
})

case = "\n".."with exp working with parathethises"

g = lpeg.P{"expression",
  primary = number + (OP * expression * CP),
  exp = space * lpeg.Ct(primary * opE * primary) / function(t) return t[1]^t[2] end
        + primary,
  term = space * Ct(exp * (opM  * exp)^0) / foldcaptures,
  expression = space * Ct(term * (opA * term)^0) / foldcaptures 
}
g = g * -1

test(case, g, {
  "1+ 1 *10 ^2",
  "(1+ 1) *10 ^2",
  "(1+ 1) *10 ^(1+2)",
  "(1+ 1) +10 ^(1*3)",
  "10 ^(1*3+1)",
  "(1+3)/.5^(1*3+1)",
})


case = "\n".."implement factorial operator; highest priority"

function factorial (n)
  --print("n",type(n),n)
  n = tonumber(n)
  if n <= 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

opFac = lpeg.C(lpeg.S("!")) * space 
fac = lpeg.V"fac"

g = lpeg.P{"expression",
  primary = number + (OP * expression * CP),
  fac = space * lpeg.Ct(primary * opFac^1) / function(t) 
                                                --print(pt(t))
                                                local acc=factorial(t[1])
                                                for i=3,#t do 
                                                  acc = factorial(acc) 
                                                end 
                                                --print("!acc",acc)
                                                return acc 
                                              end
              + primary,
  exp = space * lpeg.Ct(fac * opE * fac) / function(t) return t[1]^t[2] end
              + fac,
  term = space * Ct(exp * (opM  * exp)^0) / foldcaptures,
  expression = space * Ct(term * (opA * term)^0) / foldcaptures 
}
g = g * -1

    
test(case, g, {
  {"3!", factorial(3)},
  {"(3!+1)! * .5^3", factorial(factorial(3)+1) * .5^3},
  {"(3!+1)! * 2^((1.5+ .5)!)!", factorial( factorial(3)+1) * 2^factorial(factorial(1.5+ .5))   },
  {"(3!+1)! * 2^((1.5+ -.5)!)!", factorial(factorial(3)+1) * 2^factorial(factorial(1.5+ -.5)) },
  {"(2!+1)!!",  factorial( factorial(factorial(2)+1)) } 
})


case = "\n".."implement a zone number type and overload arithmetic on it"

-- overload arithmetic operators on zone type
local function tozone(t)
  local convert = function(a) return type(a)=="number" and {a,a} or a end -- promote single number to a zone
  return setmetatable(t, {
    __add = function(z1,z2) z1, z2 = convert(z1), convert(z2)
      return tozone({z1[1]+z2[1], z1[2]+z2[2]}) end,
    __sub = function(z1,z2) z1, z2 = convert(z1), convert(z2)
      return tozone({z1[1]-z2[1], z1[2]-z2[2]}) end,
    __tostring = function(z) return table.concat(z,":") end,
  })
end

z = tozone{1,5} + 10 - tozone{5,0}
print("\n".."test zone --> z=",z,"===>",tozone{1+10-5,5+10-0})

zone   = lpeg.Ct(number * ":" * number) / tozone * space 
arithmetical = zone + number

g = lpeg.P{"expression",
  primary = arithmetical + (OP * expression * CP),
  fac = space * lpeg.Ct(primary * opFac^1) / function(t) 
                                                --print(pt(t))
                                                local acc=factorial(t[1])
                                                for i=3,#t do 
                                                  acc = factorial(acc) 
                                                end 
                                                --print("!acc",acc)
                                                return acc 
                                              end
              + primary,
  exp = space * lpeg.Ct(fac * opE * fac) / function(t) return t[1]^t[2] end + fac,
  term = space * Ct(exp * (opM  * exp)^0) / foldcaptures,
  expression = space * Ct(term * (opA * term)^0) / foldcaptures 
}
g = g * -1

test(case, g, {
  {"5 - 1:2", "4:3"},
  {"5 + 1:2", "6:7"},
  {"5 + 1:2 - 1:2 ", "5:5"},
  {"5 + 1:2 - -1:2 ", "7:5"},
  {"5 + 1:2 - -1:(2^2) ", "7:3"}, -- LEARN: get zone working with expressions
})

print("\n".."LPEG possessiveness end of WEEK1 inclass exercise")
print("regex",string.match("AAABAAA:B", ".*:") )  --> AAABAAA:
p = lpeg.C(lpeg.P(1-lpeg.P(":"))^0 * ":" )^0
print("lpeg", lpeg.match(p,"AAABAAA:B"))

function everythingbut(set) return lpeg.P(1) - lpeg.S(set) end
function tonotS(set) return lpeg.P(everythingbut(set))^0 * lpeg.S(set) end
set = ":;,"
p = lpeg.C(tonotS(set))
print("\n".."tonotS(p,'"..set.."')")
print("lpeg", lpeg.match(p,"AAABAAA:B"))
print("lpeg", lpeg.match(p,"AAAB;AAAB"))
print("lpeg", lpeg.match(p,"AAABAA,AB"))
print("lpeg", lpeg.match(p,":"))

-- IMPROVE: use loc = lpeg.locale() e.g. lpeg.P(loc.space,"  ") loc.alnum ... alpha, cntrl, digit, graph, lower, print, punct, upper, xdigit

--]]
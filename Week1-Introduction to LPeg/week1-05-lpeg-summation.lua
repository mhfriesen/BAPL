local lpeg = require"lpeg"
nl = "\n"

print(string.match("AAABAAA:B", ".*:") )  --> AAABAAA:
  
  --

local function pt (x, id, visited)
  visited = visited or {}
  id = id or ""
  if type(x) == "string" then return "'" .. tostring(x) .. "'"
  elseif type(x) ~= "table" then return tostring(x)
  elseif visited[x] then return "..."    -- cycle
  else
    visited[x] = true
    local s = id .. "{"
    for k,v in pairs(x) do
      s = s .. id .. tostring(k) .. " = " .. pt(v, id .. "  ", visited) .. ";\n"
    end
    if 1 < #s then s = string.sub(s,1,#s-1) end
    s = s .. id .. "}"
    return s
  end
end

-- gmatch a bunch of strings
function test(name, pattern, strings)
  print("\n"..name)
  for _,str in ipairs(strings) do
    local results = {pattern:match(str)}
    if 7 < string.len(str) then
      io.write(str.."\t") 
    else
      io.write(str.."\t\t")
    end
    if #results == 0 then
        io.write("nil")
    else
      for _,r in ipairs(results) do
        if type(r) == nil then
          io.write("nil")
        else
          io.write(tostring(r))
        end
        io.write("\t")
      end
    end
    io.write(nl)
  end
end

function ismatch(str, pattern)
  local result = pattern:match(str)
  return result and result == #str + 1
end

-- 
function identify(strings)
  for _,str in ipairs(strings) do
    if ismatch(str,identifier) then
      print(str.."\tis an identifier")
    elseif ismatch(str, object) then
      print(str.."\tis an object")
    else
      print(str.."\tis not recognized")
    end
  end
end

function join(t1,t2)
  return table.move(t1,1,#t1,#t2+1, t2)
end

--[[
print("Ct nil table")
print(Ct({nil}))
print(Ct({"Ct not nil table"}))

underscore = lpeg.P("_")
alpha = lpeg.R("az") + lpeg.R("AZ")
numeric = lpeg.R("09")
alpha_numeric = alpha + numeric
identifier = (underscore + alpha) * (alpha_numeric + underscore)^0
object = identifier * lpeg.P(".") * identifier

identifiers = {"_", "0", "_0", "_A", "abc","a_b","myID_","_MYid_","M","a."}
objects = {"abc","abc.",".a","_._", "_.0", "A.B", "0.a"}

test("match indentifiers", identifier, identifiers)
test("match objects", object, objects)

identify(join(objects, identifiers)) 
--]]


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

space = lpeg.S(" \t\n")^0
operator = lpeg.P("+") * space
numeral = lpeg.R("09")^1
period = lpeg.P(".")
number =  (numeral * period * numeral +
          numeral * period  + 
          period * numeral  + 
          numeral  
          )
        
p = space * numeral  * numeral * (operator * numeral * space)^0 


case = nl .."summation pattern match a non-emCty list of numberals intercalated with + and allow spaces"
summations = {"1+1", " 1+1", " 1+1 ", "1 +1", "1+ 1","1 + 1","abc","","1+2+3","1+2 +3","\t1\t+\t2\n","1.+2"}
p = space * numeral * space * (operator * numeral * space)^0
test(case,p,summations)

case= nl .. "per above but with decimal point"
summations = {"1+10", " 10+1", " 10+1 ", "1 +10", "10+ 1","10 + 1","abc10","","1+20+3","1+2 +03","\t1\t+\t2\n","1.+2",".1+3.","1.1 + 3.2"}
p = space * number * (operator * number)^0
test(case,p,summations)

case = nl .. "per above but with +-*/ operators"
summations = {"12+13+25", "12 + 13+  25 ", " 10*1", " 10/1 ", "1. -.10", "10+ 11 a","10 + 12","abc10","","11+20+99","100+200 +03","\t1\t+\t2\n","1.+2",".1+3.","1.1 + 3.2"}
operator = lpeg.S("+-*/") * space
p = space * number * (operator * number)^0
test(case,p,summations)

case = nl .. "per above caCturing numbers and positions of operators"
capture = lpeg.C
p = space * capture(number) * (lpeg.Cp(0) * operator * capture(number))^1
test(case,p,summations)

case = nl .. "per above when the whole subject"
p = space * capture(number) * (lpeg.Cp(0) * operator * capture(number))^1 * -1
test(case,p,summations)



case = nl .. "accummulate numbers"
function add(acc, newvalue) return acc + newvalue end
numberv = lpeg.R("09")^1 / tonumber
p = space * numberv * ("+" * numberv)^1 / add * -1
test(case,p,{"12+13"})

sign = lpeg.S("+-")^0
number =  sign * (numeral * period * numeral +
          numeral * period  + 
          period * numeral  + 
          numeral  
          ) * space / tonumber -- / func makes it a capture also
        
case = nl .. "numbers can have signs and are always captured and tonumber'd"
p = space * number * (operator * number)^0 / add * -1
test(case,p,{" 12+ -13", " -5.2 - +1 "})

number =  space * sign * (numeral * period * numeral +
          numeral * period  + 
          period * numeral  + 
          numeral  
          ) / tonumber -- / func makes it a capture also
case = nl .. "SPACING different: numbers can have signs and are always captured and tonumber'd"
p =  space * number * (operator * number)^0 / add * -1
test(case,p,{" 12+ -13", " -5.2 - +1 "})

--[[

function fold(t)
  local acc = t[1]
  for i=2, #t do
    acc = acc + t[i]
  end
  return acc
end

case = nl .. "fold captures in table"
Ct = lpeg.Ct
p = space * Ct(number * (operator * number)^0) / fold * -1
test(case,p,{" 12+ -13 - 2", " -5.2 - +1 - .1"})


function foldcaptures1(t, f)
  local acc = t[1]
  for i=3, #t, 2 do
    --print("---", i, acc, t[i-1], t[i])
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

function foldcaptures(t)
  --print("foldcaptures", pt(t))
  return fold3(t, function(acc, op, n) 
      return opfuncs[op](acc, n)
  end)
end
    

operator = lpeg.C(lpeg.S("+-*/")) * space -- also capture operators

case = nl .. "acct for minus operator"

p = space * Ct(number * (operator * number)^0) / foldcaptures1 * -1
test(case,p,{" 12+ -13 - 2", " -5.2 + 1 - .1"})

case = nl .. "generalize folding"
p = space * Ct(number * (operator * number)^0) / foldcaptures * -1
test(case,p,{" 12+ -13 - 2", " -5.2 + 1 - .1"})

opA = lpeg.C(lpeg.S("+-")) * space 
opM = lpeg.C(lpeg.S("*/%")) * space 
opE = lpeg.P("^") * space
exp = space * lpeg.Ct(number * opE * number) / function(t) return t[1]^t[2] end + number
term = space * Ct(exp * (opM * exp)^0) / foldcaptures
sum = space * Ct(term * (opA * term)^0) / foldcaptures * -1
case = nl .. "exp"
print("exp", exp:match"10^3")
case = nl .. "mult and div with precedence"
test(case,sum,{" 12+ -13 - 2", " -5.2 + 1 - .1", "2 + 14 * 2", "2 + 14 * 2/2 -30","3%2 * 5 + 10^2 *3"})

print("grammer")

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

print(g:match("1+1 *10 ^2"))

print(nl.."with parathethises")

OP = lpeg.P"(" * space
CP = lpeg.P")" * space

g = lpeg.P{"expression",
  exp = space * lpeg.Ct(number * opE * number) / function(t) return t[1]^t[2] end + number,
  primary = exp + OP * expression * CP,
  term = space * Ct(primary * (opM * primary)^0) / foldcaptures,
  expression = space * Ct(term * (opA * term)^0) / foldcaptures 
}
g = g * -1

print(g:match"1+ 1 *10 ^2")
print(g:match"(1+ 1) *10 ^2")

print(nl.."with exp working with parathethises")

g = lpeg.P{"expression",
  primary = number + (OP * expression * CP),
  exp = space * lpeg.Ct(primary * opE * primary) / function(t) return t[1]^t[2] end + primary,
  term = space * Ct(exp * (opM  * exp)^0) / foldcaptures,
  expression = space * Ct(term * (opA * term)^0) / foldcaptures 
}
g = g * -1

print(g:match"1+ 1 *10 ^2")
print(g:match"(1+ 1) *10 ^2")
print(g:match"(1+ 1) *10 ^(1+2)")
print(g:match"(1+ 1) +10 ^(1*3)")
print(g:match"10 ^(1*3+1)")
print(g:match"(1+3)/.5^(1*3+1)")
print(4 / .5^4)


print(nl.."impl factorial operator; highest priority")

-- defines a factorial function
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
  exp = space * lpeg.Ct(fac * opE * fac) / function(t) return t[1]^t[2] end + fac,
  term = space * Ct(exp * (opM  * exp)^0) / foldcaptures,
  expression = space * Ct(term * (opA * term)^0) / foldcaptures 
}
g = g * -1



print(g:match"(3!+1)! * .5^3", " --> ", factorial(factorial(3)+1) * .5^3)  --> 630
print(g:match"(3!+1)! * 2^((1.5+ .5)!)!", " --> ", factorial(factorial(3)+1) * 2^factorial(factorial(1.5+ .5)) ) --> 20160.0
print(g:match"(3!+1)! * 2^((1.5+ -.5)!)!", " --> ", factorial(factorial(3)+1) * 2^factorial(factorial(1.5+ -.5)) ) --> 10080.0
print(g:match"(2!+1)!!", " --> ",  factorial( factorial(factorial(2)+1)) ) --> 720.0
print(factorial(2),factorial(5), factorial(6))


print(nl.."implement a zone number type and arithmatic on it")


function tozone(t)
  local convert = function(a) return type(a)=="table" and a or {a,a} end
  return setmetatable(t, {
    __add = function(z1,z2)
      z1, z2 = convert(z1), convert(z2)
      return {z1[1]+z2[1], z1[2]+z2[2]} end,
    __sub = function(z1,z2)
      z1, z2 = convert(z1), convert(z2)
      return {z1[1]-z2[1], z1[2]-z2[2]} end
    })
end

zone   = lpeg.Ct(number * ":" * number) / tozone * space 
thing = zone + number

g = lpeg.P{"expression",
  primary = thing + (OP * expression * CP),
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

--print(pt(g:match("1:2 + 10:-20"))) --> {11,-18}
print(pt(g:match("5 - 1:2"))) --> {4,3}



--[[
print(g:match"1+ 1 *10 ^2")
print(g:match"(1+ 1) *10 ^2")
print(g:match"(1+ 1) *10 ^(1+2)")
print(g:match"(1+ 1) +10 ^(1*3)")
print(g:match"10 ^(1*3+1)")
print(g:match"(1+3)/.5^(1*3+1)")
print(g:match"(1+3!)/.5^(1*3+1)", " -->", (1+factorial(3)) / .5^4)
print(g:match"(1+(2+1)!)/.5^(1*3+1)", " -->", (1+factorial(3)) / .5^4)

--]]


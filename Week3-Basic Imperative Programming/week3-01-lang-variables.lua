local lpeg = require"lpeg"
local lfs = require"lfs" --luafilesystem
dofile(lfs.currentdir().."\\pt.lua") --table to string
dofile(lfs.currentdir().."\\test.lua") -- run use cases
loc = lpeg.locale()

function factorial (n)
  n = tonumber(n)
  if n <= 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

function exactly(pat, n) 
  local p = lpeg.P(0) * pat -- enable p to be a string
  return #(p^n) * p^-n * -p -- check(at least 6 p)  * at most 6 p * not p
end

-- overload arithmetic and relational operators on zone type
local function tozone(t)
  local convert = function(a) return type(a)=="number" and {a,a} or a end -- promote number to a zone
  return setmetatable(t, {
    __concat = function(z1,z2) z1, z2 = convert(z1), convert(z2)
      return tozone({math.min(z1[1],z2[1]), math.max(z1[2],z2[2])}) end,
    __add = function(z1,z2) z1, z2 = convert(z1), convert(z2)
      return tozone({z1[1]+z2[1], z1[2]+z2[2]}) end,
    __sub = function(z1,z2) z1, z2 = convert(z1), convert(z2)
      return tozone({z1[1]-z2[1], z1[2]-z2[2]}) end,
    __unm = function(z) return tozone({-z[2], -z[1]}) end,
    __eq = function(z1,z2) z1, z2 = convert(z1), convert(z2)
      return z1[1]==z2[1] and z1[2]==z2[2] end,
    __lt = function(z1,z2) z1, z2 = convert(z1), convert(z2)
      return z1[1]<z2[1] end,
    __tostring = function(z) return table.concat(z,":") end,
  })
end



--FRONT END: parse input and make ast
local function nodeNum(num)
  return {tag = "number", val = tonumber(num)}
end

local function nodeVar(var)
  return {tag = "variable", var = var}
end

local spaces = lpeg.S(" \t\n")
local space = spaces^0
local digits = lpeg.R("09")^1
local hexdigits = lpeg.R("09","AF","af")
local dot = lpeg.P(".")
local hex = "0" * lpeg.S("xX") * hexdigits^1 
local decimal = digits * dot * digits +
                digits * dot  + 
                dot * digits +
                digits
local scientific = decimal *  lpeg.S("eE") * ((exactly("-",1) * digits) + digits)
local number = lpeg.C(scientific + hex + decimal) / nodeNum * space 


local underscore = lpeg.P("_")
local alpha = lpeg.R("az") + lpeg.R("AZ")
local numeric = lpeg.R("09")
local alpha_numeric = alpha + numeric
local identifier = (underscore + alpha) * (alpha_numeric + underscore)^0
local object = identifier * lpeg.P(".") * identifier
local ID = lpeg.C(identifier + object) * space
local var = ID / nodeVar

print(ID:match"x")

local OP = lpeg.P("(") * space      -- open parenthesis      
local CP = lpeg.P(")") * space      -- close parenthesis

-- operators from lowest to highest priority
local opA = lpeg.C(lpeg.S("+-|")) * space   --binary add, subract, concat (for zone)
local opM = lpeg.C(lpeg.S("*/%")) * space  --binary multiply, divide, remainder
local opZ = lpeg.C(lpeg.P(":")) * space    --binary zone
local opE = lpeg.C(lpeg.S("^")) * space    --binary exponent
local opS = lpeg.C(lpeg.S("!")) * space    --unuary suffix factorial
local opP = lpeg.C(lpeg.S("-"))            --unuary prefix negation

-- "<", ">", ">=", "<=", "==", and "!="
local opR = lpeg.C(lpeg.P("<=") + ">=" + "~=" + "==" + "<" + ">") * space

-- convert captured list of number op number ... into a tree
function foldBin(lst)
  local tree = lst[1]
  for i=2, #lst, 2 do
    tree = { tag = "binop", e1 = tree, op = lst[i], e2 = lst[i+1] }
  end
  return tree
end

function foldSuf(lst)
  local tree = lst[1]
  for i=2, #lst do
    tree = { tag = "sufop", e1 = tree, op = lst[i] }
  end
  return tree
end

function foldPre(lst)
  local tree = lst[#lst]
  for i=1, #lst-1 do
    tree = { tag = "preop", e1 = tree, op = lst[i] }
  end
  return tree
end

local factor = lpeg.V"factor"
local prefix = lpeg.V"prefix"
local zone = lpeg.V"zone"
local suffix = lpeg.V"suffix"
local expo = lpeg.V"expo"
local term = lpeg.V"term"
local expr = lpeg.V"expr"
local relation = lpeg.V"relation"

local grammer = lpeg.P{"relation",
  factor = number + OP * relation * CP + var,
  prefix = factor + lpeg.Ct(((spaces + 0) * opP)^1 * factor) / foldPre,
  zone = lpeg.Ct(prefix * (opZ  * prefix)^0 ) / foldBin  * space,
  suffix = lpeg.Ct(zone * opS^0) / foldSuf * space,
  expo = lpeg.Ct(suffix * (opE * suffix)^0) / foldBin  * space,
  term = lpeg.Ct(expo * (opM * expo)^0 ) / foldBin  * space,
  expr = lpeg.Ct(term * (opA  * term)^0 ) / foldBin  * space,
  relation = lpeg.Ct(expr * (opR  * expr)^0) / foldBin  * space
}
grammer = space * grammer * -1

local function parse(input, options)
  options = options and options or {}
  options.grammer = options.grammer and options.grammer or grammer
  return options.grammer:match(input)
end

--COMPILER: consume ast and generate code
local function addCode(state, op)
  local code = state.code
  code[#code + 1] = op
end

local binOps = {
  ["<"] = "lt", [">"] = "gt", ["<="] = "le", [">="] = "ge", ["=="] = "eq", ["~="] = "ne", 
  ["+"] = "add", ["-"] = "sub", 
  ["*"] = "mul", ["/"] = "div", ["%"] = "rem", ["|"] = "con",
  ["^"] = "exp",
  [":"] = "zone",
}
local preOps = {
  ["-"] = "neg",
}

local sufOps = {
  ["!"] = "fac",
}

local function codeExp(state, ast)
  if ast.tag == "number" then
    addCode(state, "push")
    addCode(state, ast.val)
  elseif ast.tag == "binop" then
    codeExp(state, ast.e1)
    codeExp(state, ast.e2)
    addCode(state, binOps[ast.op])
  elseif ast.tag == "preop" then
    codeExp(state, ast.e1)
    addCode(state, preOps[ast.op])
  elseif ast.tag == "sufop" then
    codeExp(state, ast.e1)
    addCode(state, sufOps[ast.op])
  elseif ast.tag == "variable" then
    addCode(state, "var")
    addCode(state, ast.var)
  else error("invalid tree")
  end
end

local function compile(ast)
  local state = {code={}}
  codeExp(state, ast)
  return state.code
end

--VIRTUAL MACHINE: execute generated code
local function run(code, mem, stack, options)
  options = options and options or {}
  prnt = options.trace and io.write or function() end
  prnt("\ninterpret\n")
  local pc = 1
  local top = 0
  
  local function ps()
    local s = " ["
    for i=1,top do
      s = s .. tostring(stack[i]) .. " "
    end
    return s .. "]"
  end
      
  while pc <= #code do -- guessing pc will come in handy when interpreting loops that jump back to earlier code
    prnt(code[pc], ps())
    if code[pc] == "push" then
      pc = pc + 1
      top = top + 1
      stack[top] = code[pc]
    elseif code[pc] == "var" then 
      pc = pc + 1
      local id = code[pc]
      top = top + 1
      stack[top] = mem[id]
     -- BIN OPS do op on top 2 nums replacing top of reduced stack with the result
     elseif code[pc] == "add" then    
      stack[top-1] = stack[top-1] + stack[top] 
      top = top - 1
    elseif code[pc] == "sub" then
      stack[top-1] = stack[top-1] - stack[top] 
      top = top - 1
    elseif code[pc] == "mul" then 
      stack[top-1] = stack[top-1] * stack[top] 
      top = top - 1
    elseif code[pc] == "div" then 
      stack[top-1] = stack[top-1] / stack[top] 
      top = top - 1
    elseif code[pc] == "rem" then 
      stack[top-1] = math.fmod(stack[top-1], stack[top])
      top = top - 1
    elseif code[pc] == "exp" then 
      stack[top-1] = stack[top-1] ^ stack[top]
      top = top - 1
    elseif code[pc] == "zone" then 
      stack[top-1] = tozone({stack[top-1], stack[top]})
      top = top - 1
    elseif code[pc] == "lt" then 
      stack[top-1] = stack[top-1] < stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "gt" then 
      stack[top-1] = stack[top-1] > stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "eq" then 
      stack[top-1] = stack[top-1] == stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "ne" then 
      stack[top-1] = stack[top-1] ~= stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "ge" then 
      stack[top-1] = stack[top-1] >= stack[top] and 1 or 0
      top = top - 1
    elseif code[pc] == "le" then 
      stack[top-1] = stack[top-1] <= stack[top] and 1 or 0
      top = top - 1
     elseif code[pc] == "con" then    
      stack[top-1] = stack[top-1] .. stack[top] 
      top = top - 1
     -- SUF OPS replace top of stack with result of op on top of stack
    elseif code[pc] == "fac" then -- pop number and factorial
      stack[top] = factorial(stack[top]) 
     -- PRE OPS
    elseif code[pc] == "neg" then -- per above
      stack[top] = -stack[top]
    else error("unkown instruction")
    end
    prnt(ps()); prnt("\n")
    pc = pc + 1
  end
end

--USER 
function pl(cases, options)
  options = options and options or {}
  for _,case in ipairs(cases) do   
    io.write("CASE--->"..case)
    local ast = parse(case, options)
    print("\nast"); print(pt(ast))
    local code = compile(ast)
    print("code"); print(pt(code))
    local stack = {}
    local mem = {x=10, y=100, _x=1000}
    run(code,mem, stack,options)
    print("--->"..tostring(stack[1]))
  end
end

print"\nsomewhat ridiculous pl (ESPL) user cases\n"
print("\ntrace GRAMMER")
pl({"2 + x + y + _x"},{trace=true, grammer=grammer})

--[[
print("GRAMMER")
pl({
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
   "-.1:0.2 |-0.2:.3",
   "-(-.1:0.2 |-0.2:.3)",
   "-0X02:0xff| 2e10-2e1",
   "0x02 + 0X03",
   "0x02c + 0X03", 
   "--1",
   "-(-1)", 
   "1- -3",
   "5 - - -1",
   "5-1",
   "-5-1",
   "2^2^2",
},{grammer=grammer})
--]]

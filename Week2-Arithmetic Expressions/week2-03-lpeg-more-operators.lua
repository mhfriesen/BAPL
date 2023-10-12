local lpeg = require"lpeg"
local lfs = require"lfs" --luafilesystem
dofile(lfs.currentdir().."\\pt.lua") --table to string
dofile(lfs.currentdir().."\\test.lua") --table to string
loc = lpeg.locale()

--FRONT END: parse input and make ast
local function node(num)
  --print ("node", num)
  return {tag = "number", val = tonumber("0"..num)}
end

local space = lpeg.S(" \t\n")^0
local numeral = lpeg.R("09")^1 * space

hexdigit = lpeg.R("09","AF","af")
function exactly(pat, n) 
  local p = lpeg.P(0) * pat -- enable p to be a string
  return #(p^n) * p^-n * -p -- check(at least 6 p)  * at most 6 p * not p
end 
local hex = lpeg.S("xX") * exactly(hexdigit,2) * space 
case = "hex matching"
test(case, hex, {
    "xfF",
    "xfg",
    "xf",
    "xfFa",
    "x00",
})

numorhex = (hex + numeral) / node * space

local opA = lpeg.C(lpeg.S("+-")) * space
local opM = lpeg.C(lpeg.S("*/")) * space
local opE = lpeg.C(lpeg.P("^")) * space

-- convert captured list of numorhex op numberhex ... into a tree
function foldBin(lst)
  local tree = lst[1]
  for i=2, #lst, 2 do
    tree = { tag = "binop", e1 = tree, op = lst[i], e2 = lst[i+1] }
  end
  return tree
end

local exponential = lpeg.Ct(numorhex * (opE * numorhex)^0) / foldBin  * space
local term = lpeg.Ct(exponential * (opM * exponential)^0) / foldBin  * space
local exp = lpeg.Ct(term * (opA  * term)^0) / foldBin  * space

local function parse(input)
  return exponential:match(input)
end

--COMPILER: consume ast and generate code
local function addCode(state, op)
  local code = state.code
  code[#code + 1] = op
end

ops = {
  ["+"] = "add", ["-"] = "sub", 
  ["*"] = "mul", ["/"] = "div", ["%"] = "rem",
  ["^"] = "exp"
}

local function codeExp(state, ast)
  if ast.tag == "number" then
    addCode(state, "push")
    addCode(state, ast.val)
  elseif ast.tag == "binop" then
    codeExp(state, ast.e1)
    codeExp(state, ast.e2)
    addCode(state, ops[ast.op])
  else error("invalid tree")
  end
end

local function compile(ast)
  local state = {code={}}
  codeExp(state, ast)
  return state.code
end

--VIRTUAL MACHINE: execute generated code
local function run(code, stack)
  local pc = 1
  local top = 0
  while pc <= #code do
    if code[pc] == "push" then
      pc = pc + 1
      top = top + 1
      print("push", code[pc])
      stack[top] = code[pc]
    elseif code[pc] == "add" then 
      print("add", stack[top-1], stack[top] )
      stack[top-1] = stack[top-1] + stack[top] 
      top = top - 1
    elseif code[pc] == "sub" then -- pop 2 numbers and add
      print("sub", stack[top-1], stack[top] )
      stack[top-1] = stack[top-1] - stack[top] 
      top = top - 1
    elseif code[pc] == "mul" then -- pop 2 numbers and add
      print("mul", stack[top-1], stack[top] )
      stack[top-1] = stack[top-1] * stack[top] 
      top = top - 1
    elseif code[pc] == "div" then -- pop 2 numbers and add
      print("div", stack[top-1], stack[top] )
      stack[top-1] = stack[top-1] / stack[top] 
      top = top - 1
    elseif code[pc] == "rem" then -- pop 2 numbers and add
      print("rem", stack[top-1], stack[top] )
      stack[top-1] = math.fmod(stack[top-1], stack[top])
      top = top - 1
    elseif code[pc] == "exp" then -- pop 2 numbers and add
      print("exp", stack[top-1], stack[top] )
      stack[top-1] = math.fmod(stack[top-1], stack[top])
      top = top - 1
    else error("unkown instruction")
  end
  pc = pc + 1
  end
end

--USER 
print"\nextremely simple pl (ESPL) user cases"
for _,case in ipairs {
   "11%4",
   "3",
   "2^3 * 2",
   "16"
  } do
  print("case:"..case)
  local ast = parse(case)
  print("ast")
  print(pt(ast))
  local code = compile(ast)
  print("code")
  print(pt(code))
  local stack = {}
  run(code,stack)
  print("--->"..tostring(stack[1]))
end

--]]

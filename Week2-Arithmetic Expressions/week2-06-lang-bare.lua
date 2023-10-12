local lpeg = require"lpeg"
local lfs = require"lfs" --luafilesystem
dofile(lfs.currentdir().."\\pt.lua") --table to string
dofile(lfs.currentdir().."\\test.lua") --table to string
loc = lpeg.locale()

--FRONT END: parse input and make ast
local function node(num)
  print ("node", num)
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

local function parse(input)
  return numorhex:match(input)
end

--COMPILER: consume ast and generate code
local function compile(ast)
  if ast.tag == "number" then -- assumes ast just has one node TODO: accept multiple nodes
    return {"push", ast.val}
  end
end

--VIRTUAL MACHINE: execute generated code
local function run(code, stack)
  local pc = 1
  local top = 0
  while pc <= #code do
    if code[pc] == "push" then
      pc = pc + 1
      top = top +1
      stack[top] = code[pc]
    else error("unkown instruction")
    end
    pc = pc + 1
  end
end

--USER 
print"\nridiculously simple pl (RSPL) user cases"
for _,case in ipairs {
   "xFf",
   "45"
  } do
  print("case:"..case)
  local ast = parse(case)
  print("ast",pt(ast))
  local code = compile(ast)
  print("code",pt(code))
  local stack = {}
  run(code,stack)
  print("--->"..tostring(stack[1]))
end

--]]

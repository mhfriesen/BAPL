local lpeg = require"lpeg"
local lfs = require"lfs" --luafilesystem
dofile(lfs.currentdir().."\\pt.lua") --table to string

--print(string.byte([[\n
--]])
--)
--os.exit()

local function pairsByKeys (t, f)
  local a = {}
  for n in pairs(t) do table.insert(a, n) end
  table.sort(a, f)
  local i = 0      -- iterator variable
  local iter = function ()   -- iterator function
    i = i + 1
    if a[i] == nil then return nil
    else return a[i], t[a[i]]
    end
  end
  return iter
end

local function factorial (n)
  n = tonumber(n)
  if n <= 0 then
    return 1
  else
    return n * factorial(n-1)
  end
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

--------------------------------------FRONT END: parse input and make ast

--lpeg generator
local function exactly(pat, n) 
  local p = lpeg.P(0) * pat -- enable p to be a string
  return #(p^n) * p^-n * -p -- check(at least 6 p)  * at most 6 p * not p
end

local function I(msg)
  return lpeg.P(function(s,p) print(msg.." p="..tostring(p)) return true end) 
end

--generate lines_range table using lpeg (for syntax msgs)
local function calc_lines_range(str)
  --print(str)
  local lines_range = { }
  local p1 = 1
  local tally = lpeg.P(function(s,p)
    --print("p",p)
    table.insert(lines_range, {p1, p-1})
    p1 = p 
    return true 
  end)
  local pat = ( (lpeg.P(1) * -lpeg.P("\n"))^1 * (lpeg.P(1) * ((lpeg.P("\n") * tally))^0 + (lpeg.P(1)) * tally) )^1 * -1
  pat:match(str)
  --table.remove(lines_range)
  return lines_range
end

--print("lines_range",pt(calc_lines_range("123\n567\n\n0123456\n")))
--os.exit()

--primordial patterns 
function primordial_patterns_start() end -- for IDE outline view
local spaces = lpeg.S(" \t\n")^1

local p_maxmatch = 0 -- farthest position lpeg has succeeded matching so far
local lines = {};
local space = #lpeg.P( -- lpeg match function to track how far matching succeeds and new lines
  function(s,p)
    local s1 = string.sub(s,p,p)
    --print(" s1=".."'"..s1.."'".." char="..(string.byte(s1) and string.byte(s1) or "?").." p="..tostring(p).." near="..string.sub(s, math.max(1,p-10),p))
    if s1=="\n" then
      lines[p] = lines[p] and lines[p] + 1 or 1 
    end
    if lpeg.S(" \n\t"):match(s1) then
      p_maxmatch = math.max(p_maxmatch, p);
    end
    return true 
 end
) * lpeg.S(" \t\n")^0

local keywords = (
  lpeg.P"not" 
      + "inc" 
      + "dec"
      + "return") * spaces

--abstract syntax tree building functions called recursively by grammer

local function nodeNum(num)
  return {tag = "number", val = tonumber(num)}
end

local function nodeVar(var)
  --print("\nnodeVar"); print("var=",var)
  if keywords:match(var) then error("vars cannot be a reserved word " .. var) end --TODO: handle var.member pattern
  return {tag = "variable", var = var}
end

local function nodeAssgn(id, expr)
  --print("\nnodeAssgn"); print("id=",pt(id)); print("expr=",expr)
  if keywords:match(id) then error("cannot assign to reserved word " .. id) end
  return {tag = "assgn", id = id, expr = expr}
end

local function nodeSeq(stmt1, stmt2)
  if stmt2 == nil then
    return stmt1
  else
    return {tag = "seq", stmt1 = stmt1, stmt2 = stmt2}
  end
end

local function nodeReturn(expr)
  return {tag = "return", expr = expr}
end

local function nodePrint(expr)
  return {tag = "print", expr = expr}
end


-- convert captured lists into a tree
local function foldBin(lst)
  local tree = lst[1]
  for i=2, #lst, 2 do
    tree = { tag = "binop", e1 = tree, op = lst[i], e2 = lst[i+1] }
  end
  return tree
end

local function foldRight(lst)
  local tree = lst[#lst]
  for i = #lst-1, 1, -2 do
    tree = { tag = "rightop", e1 = tree, op = lst[i], e2 = lst[i-1] }
  end
  return tree
end

local function foldSuf(lst)
  local tree = lst[1]
  for i=2, #lst do
    tree = { tag = "sufop", e1 = tree, op = lst[i] }
  end
  return tree
end

local function foldPre(lst)
  --print("\nfoldPre"); print("lst:",pt(lst))
  local tree = lst[#lst]
  for i = #lst-1, 1, -1 do
    tree = { tag = "preop", e1 = tree, op = lst[i] }
  end
  --print("tree:",pt(lst))
  return tree
end

--GRAMMER

local digits = lpeg.R("09")^1
local hexdigits = lpeg.R("09","AF","af")
local dot = lpeg.P(".")
local hex = "0" * lpeg.S("xX") * hexdigits^1 
local decimal = digits * dot * digits +
                digits * dot  + 
                dot * digits +
                digits
local scientific = decimal *  lpeg.S("eE") * ((exactly("-",1) * digits) + digits)
local number = (scientific + hex + decimal) / nodeNum * space 

local Assgn = "=" * space
local SC = ";" * space

local ret = "return" * space
local prnt = "@" * space

local underscore = lpeg.P("_")
local alpha = lpeg.R("az") + lpeg.R("AZ")
local numeric = lpeg.R("09")
local alpha_numeric = alpha + numeric

local identifier =  (underscore + alpha) * (alpha_numeric + underscore)^0
local object = identifier * lpeg.P(".") * identifier
local ID = lpeg.C(identifier + object) * space
local var = ID / nodeVar -- reserved words are rejected in nodeVar

local OP = lpeg.P("(") * space      -- open parenthesis      
local CP = lpeg.P(")") * space      -- close parenthesis
local OB = lpeg.P("{") * space      -- open braces      
local CB = lpeg.P("}") * space      -- close braces

-- operators from lowest to highest priority
local opA = lpeg.C(lpeg.S("+-|")) * space  --binary left associative add, subract, concat (for zone)
local opM = lpeg.C(lpeg.S("*/%")) * space  --binary left assocociative multiply, divide, remainder
local opZ = lpeg.C(lpeg.P(":")) * space    --binary left associative zone
local opE = lpeg.C(lpeg.S("^")) * space    --binary right associative exponent
local opS = lpeg.C(lpeg.S("!")) * space    --unuary suffix left associative factorial
local opP = lpeg.C(lpeg.S("-+")) * space   --unuary right associative, spaces optional           
local opPs= lpeg.C(                        --unuary right associative, spaces mandatory (could be an ID otherwise)
            lpeg.P"not"       
                + "dec"
                + "inc"
                + "return"
            ) * spaces 

-- "<", ">", ">=", "<=", "==", and "!="
local opR = lpeg.C(lpeg.P("<=") + ">=" + "~=" + "==" + "<" + ">") * space

local stmt = lpeg.V"stmt"
local stmts = lpeg.V"stmts"
local block = lpeg.V"block"
local factor = lpeg.V"factor"
local prefix = lpeg.V"prefix"
local zone = lpeg.V"zone"
local suffix = lpeg.V"suffix"
local expo = lpeg.V"expo"
local term = lpeg.V"term"
local expr = lpeg.V"expr"
local relation = lpeg.V"relation"
local notty = lpeg.V"notty"

function grammer_start() end -- for IDE outline view
local grammer = lpeg.P{"stmts",
  stmts = stmt * (SC * stmts)^-1 / nodeSeq,
  block = OB * (stmts) * SC^-1 * CB,
  stmt = OB * CB -- empty block
      + block 
      + (ID + keywords) * Assgn * notty / nodeAssgn 
      + ret * notty / nodeReturn -- return
      + prnt * notty / nodePrint -- print (don't make a unary op because print(print( )) is undesirable
                                    -- TODO: capture an optional string to prefix output e.g. "x="
      + space, -- empty stmt TODO: would a SC work?
  factor = number + OP * notty * CP + var, -- allow keywords in var but trap in ast creation assgnNode
  prefix = lpeg.Ct((opP + opPs)^1  * factor)  / foldPre  -- prefix ops (except 'not' trapped below) trap here before nodeVar sees them 
           + factor,                                     -- but nodeVar traps the others in expressions below
  zone = lpeg.Ct(prefix * (opZ  * prefix)^0 ) / foldBin  * space,
  suffix = lpeg.Ct(zone * opS^0) / foldSuf * space,
  expo = lpeg.Ct(suffix * (opE * suffix)^0) / foldRight  * space,
  term = lpeg.Ct(expo * (opM * expo)^0 ) / foldBin  * space,
  expr = lpeg.Ct(term * (opA  * term)^0 ) / foldBin  * space,
  relation = lpeg.Ct(expr * (opR  * expr)^0) / foldBin  * space,
  notty =  lpeg.Ct((lpeg.C"not" * spaces)^1 *  relation) / foldPre -- prefix operator 'not' lowest priority for applying to relations
          + relation
}
grammer = space * grammer * -1
  
-- parse
function parse_start() end -- for IDE outline view
local function parse(input, options)
  options = options and options or {}
  options.grammer = options.grammer and options.grammer or grammer --enable experimenting with multiple grammers
  local r = options.grammer:match(input) 
  return r
end

--------------------------------------COMPILER: consume ast and generate code
--left associative binary ops
local binOps = {
  ["<"] = "lt", [">"] = "gt", ["<="] = "le", [">="] = "ge", ["=="] = "eq", ["~="] = "ne", 
  ["+"] = "add", ["-"] = "sub", 
  ["*"] = "mul", ["/"] = "div", ["%"] = "rem", ["|"] = "con",
  [":"] = "zone",
}

--right associative binary ops
local rightOps = {
  ["^"] = "exp",
}

-- prefix ops, space optional or mandatory; not is lowest priority, the others prefix priority
local preOps = {
  ["-"] = "neg",
  ["+"] = "pos",
  ["dec"] = "decr",
  ["inc"] = "incr",
  
  ["not"] = "not",
}

-- repeatable suffix ops, space optional
local sufOps = {
  ["!"] = "fac",
}

-- convert variable names to an index into list they are stored in (for direct lookup)
local function var2num(state, id)
  local num = state.vars[id]
  if not num then
    num = state.nvars + 1
    state.nvars = num
    state.vars[id] = num
  end
  return num
end

-- recursively generate code from ast
local function addCode(state, op)
  local code = state.code
  code[#code + 1] = op
end

local function codeExp(state, ast)
  if ast.tag == "number" then
    addCode(state, "push")
    addCode(state, ast.val)
  elseif ast.tag == "binop" then
    codeExp(state, ast.e1)
    codeExp(state, ast.e2)
    addCode(state, binOps[ast.op])
  elseif ast.tag == "rightop" then
    codeExp(state, ast.e2)
    codeExp(state, ast.e1)
    addCode(state, rightOps[ast.op])
  --elseif ast.tag == "nottyop" then
  --  codeExp(state, ast.e1)
  --  addCode(state, nottyOps[ast.op])
  elseif ast.tag == "preop" then
    codeExp(state, ast.e1)
    addCode(state, preOps[ast.op])
  elseif ast.tag == "sufop" then
    codeExp(state, ast.e1)
    addCode(state, sufOps[ast.op])
  elseif ast.tag == "variable" then
    if not state.vars[ast.var] then
      print("\nCOMPILER ERROR: variable " .. ast.var .. " not defined")
      if keywords:match(ast.var.." ") then print("also, we cannot use the reserved word " .. ast.var .. " as a variable") end
      print()
      error("CAPL: variable " .. ast.var .. " not defined")
    end
    addCode(state, "load")
    addCode(state, var2num(state, ast.var))
  else error("CALP: invalid tree ast.tag="..ast.tag)
  end
end

-- generate code from ast statements
local function codeStmt(state, ast)
  if ast == nil or ast.tag == nil then
    -- empty stmt, do nothing
  elseif ast.tag == "assgn" then
    codeExp(state, ast.expr)
    addCode(state, "store")
    addCode(state, var2num(state, ast.id))
  elseif ast.tag == "seq" then
    codeStmt(state, ast.stmt1)
    codeStmt(state, ast.stmt2)
  elseif ast.tag == "return" then
    codeExp(state, ast.expr)
    addCode(state, "return")
  elseif ast.tag == "print" then
    codeExp(state, ast.expr)
    addCode(state, "print")
  else error("invalid tree ast.tag="..ast.tag)
  end
end

-- generate code from abstract syntax tree
local function compile(ast)
  local state = {code = {}, vars = {}, nvars = 0}
  codeStmt(state, ast)
  -- make all programs, aka code generated, end with a return stmt
  addCode(state, "push")
  addCode(state, 0)
  addCode(state, "return")
  return state.code
end

--------------------------------------VIRTUAL MACHINE: execute generated code
  local function ps(stack, top) -- print stack
    local s = " ["
    for i=1,top do
      s = s .. tostring(stack[i]) .. " "
    end
    return s .. "]"
  end

local function run(code, stack, mem, options)
  options = options and options or {}
  prnt = options.trace and io.write or function() end
  prnt("\ninterpret\n")
  local pc = 1
  local top = 0
  
  flag_print = false
  
  while true do 
    prnt(tonumber(pc).. " "..code[pc], ps(stack, top))
    if code[pc] == "return" then 
      return stack[top]
    elseif code[pc] == "print" then 
      if not flag_print then
        print()
        flag_print = true
      end
      print("  printing", stack[top])
      top = top - 1
    elseif code[pc] == "push" then
      pc = pc + 1
      top = top + 1
      stack[top] = code[pc]
    elseif code[pc] == "load" then 
      pc = pc + 1
      local id = code[pc]
      top = top + 1
      stack[top] = mem[id]
    elseif code[pc] == "store" then 
      pc = pc + 1
      local id = code[pc]
      mem[id] = stack[top]
      top = top - 1
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
    elseif code[pc] == "neg" then 
      stack[top] = -stack[top]
    elseif code[pc] == "pos" then 
    elseif code[pc] == "not" then 
      stack[top] = (not stack[top] or stack[top]==0) and 1 or 0
    elseif code[pc] == "decr" then 
      stack[top] = stack[top] - 1
    elseif code[pc] == "incr" then 
      stack[top] = stack[top] + 1
    else error("unkown instruction "..code[pc])
    end
    prnt(ps(stack, top)); prnt("\n")
    pc = pc + 1
  end
end

--------------------------------------USER: program in new language, run compiler on it, interpret it
  -- report parsing results
local function sorted_lines(lines)
  local sorted_lines = {}
  for k,v in pairsByKeys(lines) do
    table.insert(sorted_lines, k)
  end
  return sorted_lines
end

local function calc_pmax_line_and_range(pmax, lines_range)
  for i,range in ipairs(lines_range) do
    if range[1]<=pmax and pmax<=range[2] then
      return i, range
    end
  end
end


function pl(cases, options)
  options = options and options or {}
  for _,case in ipairs(cases) do   
    io.write("\nCASE--->\n",case)
    
    local ast = parse(case, options)
    
    local pmax = p_maxmatch
    print("\nGRAMMER p_maxmatch="..tostring(pmax)) 
    local lines = sorted_lines(lines)
    print("lines", table.concat(lines,","))
    local lines_range = calc_lines_range(case)
    print("lines_ranges", pt(lines_range))

    -- report any SYNTAX ERROR
    if pmax < string.len(case) then
      local lcase = string.len(case)
      local nLine, range = calc_pmax_line_and_range(pmax, lines_range)
      local preamble = "syntax error on line " .. tostring(nLine)..": "
      io.write(
        "\n", preamble, 
        string.sub(case, range[1], range[2]-1), "\n",
        string.rep(" ", string.len(preamble) + pmax - range[1] + 1), "^\n"
      )
    end
    -- optionally trace ast
    if options.trace then print("\nast"); print(pt(ast)) end
    local code = compile(ast)
    -- optionally trace code
    if options.trace then print("\ncode"); print(pt(code)) end
    -- interpret code
    local stack = {}
    local mem = {}
    print("\n--->"..tostring(run(code, stack, mem, options)))
    io.write("REPORTING lines="..tostring(#lines).. " at p=")
    io.write(table.concat(lines,","),"\n")
    --print("--->"..tostring(stack[1]))
  end
end

print"\nprimordial pl (PPL) user cases\n"
print("NEW cases")
--pl({"x = - inc -4; return x"},{trace=false, grammer=grammer})
pl({"x = - not not -2<3;" .. "\n" ..
    "@ (not -<2);"        .. "\n" ..
    "@ ((not 3)<-1);"     .. "\n" ..
    "return x\n"
},{trace=false, grammer=grammer})

--[=[
pl({[[
  x=1:3; 
  y=dec 2; 
  z= inc 4; 
  @ (x+7); 
  @ 2<3; 
  @ not (2<3); 
  return -x + y+z
]]},{trace=false, grammer=grammer})
pl({"x=1+inc 4; return x"},{trace=false, grammer=grammer})
--pl({"x=1+inc; return x"},{trace=false, grammer=grammer}) --> errors: inc not defined; don't use a reserved word
--pl({"x = y!; return x"},{trace=false, grammer=grammer}) --> error: y is not a defined variable

pl({"return 10 + inc 3:1"},{trace=false, grammer=grammer})
pl({"de =dec dec 1; return-de"},{trace=false, grammer=grammer})
pl({[[
  {x=3;;; 
  y=1:2}; 
  { }; 
  return x+y
]]},{trace=false, grammer=grammer})

pl({"return 10 + dec 3:1"},{trace=false, grammer=grammer})
pl({"return 10 + dec (3:1)"},{trace=false, grammer=grammer})

print("\nREGRESSION TESTS")
tests = {
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
   "2^3^2",
}
for i,v in ipairs(tests) do
  tests[i] = "return ".. v
end

pl(tests,{grammer=grammer})
--]=]

local lpeg = require"lpeg"
local pt = require"pt" 

-- some IDE outline views highlight non local functions; in any case CAPS standout; thus the following hack
function FUNCTIONS() end --------------------------------------FUNCTIONS: general

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

-- overload arithmetic and relational operators on zone type (hydro generator zones; similar to intervals but with subtleties to come)
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


function FRONT_END() end  --------------------------------------FRONT END: parse input and make ast

--------- PRIMORDIAL LPEG PATTERNS 
lpeg.locale(lpeg) -- adds locale entries
local P = lpeg.P 


-- lpeg debugging matchtime function pattern
local function I(msg)
  return P(function(s,p) print(msg.." p="..tostring(p)) return true end) 
end

-- generate lpeg exactly pattern
local function exactly(pat, n) 
  local p = P(0) * pat -- enable p to be a string
  return #(p^n) * p^-n * -p -- check(at least 6 p) * at most 6 p * not p
end

local nl = P("\n")     -- new line
local nnl = P(1) - nl  -- anything except a new line; i.e. not new line equiv to -#(P("\n")) * P(1)

-- SYNTAX ERROR reporting
local p_maxmatch = 0 
local function maxp(p) p_maxmatch = math.max(p_maxmatch, p) end


-- group matchtime positions (closure over previous position)
local function mt_group_positions(t) 
  local pp = 1 -- prev pos
  return P(function(s,p)
    t[#t+1] = {pp, p-1} -- group
    pp = p 
    return true 
  end)
end

--generate lines_range table using lpeg (for syntax error reporting)
local function calc_lines_range(str)
  if string.sub(str,-1) ~= "\n" then str = str.."\n" end
  local lines_range = {}
  local group = mt_group_positions(lines_range)
  local pat = ( (nl * group)^1 + nnl^1 * nl * group + nnl^1 * group )^0 * -1
  pat:match(str)
  return lines_range
end

local function calc_pmax_line_and_range(pmax, lines_range)
  for i,range in ipairs(lines_range) do
    if range[1]<=pmax and pmax<=range[2] then
      return i, range
    end
  end
end

local function report_syntax_error(case, pmax, msg, options)
  options = options or {trace = false}
  cmax = string.len(case)
  local lines_range = calc_lines_range(case)
  --print("lines_range="..pt.pt(lines_range))
  local nLine, range = calc_pmax_line_and_range(pmax, lines_range)
  print("\n\nSYNTAX ERROR (pos "..tostring(pmax).." of "..tostring(cmax)..") on line ".. tostring(nLine)..":") 
  io.write(
    string.sub(case, range[1], range[2]-1), "\n",
    string.rep(" ", pmax - range[1]), "^\n"
  )
  io.write(msg.."\n")
  if options.trace then print("lines_ranges", pt.pt(lines_range)) end
  isSyntaxErrorReported = true
end

--[[
local function report_syntax_error(case, msg, options)
  options = options or {trace = false}
  local pmax, cmax = p_maxmatch, string.len(case)
  print("\n\nSYNTAX ERROR pos "..tostring(pmax-1).." of "..tostring(cmax)) 
  io.write(msg)
  local lines_range = calc_lines_range(case)
  --print("lines_range="..pt.pt(lines_range))
  local nLine, range = calc_pmax_line_and_range(pmax, lines_range)
  if options.trace then print("lines_ranges", pt.pt(lines_range)) end
  local preamble = "on line " .. tostring(nLine)..": "
  io.write(
    preamble, 
    string.sub(case, range[1], range[2]-1), "\n",
    string.rep(" ", string.len(preamble) + pmax - range[1]), "^\n"
  )
  isSyntaxErrorReported = true
end
--]]


--------- MORE LPEG PATTERNS 
function MORE_LPEG_PATTERNS() end -- for IDE outline 

local comment = "#" * nnl^0

-- matchtime nested block comments

local function nested_block_comments() 
  local level = 0
  local function nested(s,p)
    local tag = string.sub(s,p-2,p-1)
    if  tag == "#{" then 
      level = level + 1
      p = p + 1
    elseif tag == "}#" then
      level = level -1
      if level == 0 then
        p = p - 2
        return p 
      else
        p = p + 1
      end
    else
      if string.len(s) <= p then
        report_syntax_error(s, p, "block comment closing '}*' not found\n")
        return nil
      end
      p = p + 1
    end
    maxp(p)
    return nested(s,p)
  end
  return nested
end

nested_block_comments = nested_block_comments()

local block_comment = P"#{" * nested_block_comments * P"}#"

--[=[
print(block_comment:match(
  [[#{my block comment #{ with a nested block comment # just a comment too
  here }# and rest of comment }# p=104 yadda yadda
  ]])
)
os.exit()
--print(block_comment:match("#{12 #{abc\n}#3377774}#abc"))
--]=]
  
-- use space pattern to trace how far matching gets since spaces are pervasive
local space = lpeg.V"space" -- i.e. defined in grammar for efficiency            
              
local spaces = lpeg.space^1

--------- AST BUILDING FUCTIONS (called recursively by grammar)
local ast_vars = {}
--local isSyntaxErrorReported = false 

local function nodeNum(num)
  return {tag = "number", val = tonumber(num)}
end

local function nodeVar(var)
  return {tag = "variable", var = var}
end

local function nodeAssgn(id, expr)
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
  --print("\nfoldPre"); print("lst:",pt.pt(lst))
  local tree = lst[#lst]
  for i = #lst-1, 1, -1 do
    tree = { tag = "preop", e1 = tree, op = lst[i] }
  end
  --print("tree:",pt.pt(lst))
  return tree
end

function PATTERNS() end -- for IDE outline view

local digits = lpeg.R("09")^1
local hexdigits = lpeg.R("09","AF","af")
local dot = P(".")
local hex = "0" * lpeg.S("xX") * hexdigits^1 
local decimal = digits * dot * digits +
                digits * dot  + 
                dot * digits +
                digits
local scientific = decimal *  lpeg.S("eE") * ((exactly("-",1) * digits) + digits)
local number = (scientific + hex + decimal) / nodeNum * space 

-- token pattern factories
local function T(str) return str * space end
local function Rw(word) return word * space end

local underscore = P("_")
local alpha = lpeg.R("az") + lpeg.R("AZ")
local numeric = lpeg.R("09")
local alpha_numeric = alpha + numeric

local identifier =  (underscore + alpha) * (alpha_numeric + underscore)^0
local object = identifier * P(".") * identifier
local ID = identifier + object

local keywords = {
  ["not"] = true, 
  ["inc"] = true, 
  ["dec"] = true, 
  ["return"] = true, 
}    

--trap misuses of reverved words in match time functions 
local pp -- prev pos
local closedVar -- var to be assigned
local var_closure = #(ID) * function(s,p) pp = p; return true end * lpeg.C(ID) * 
    function(s,p)
      closedVar = string.sub(s, pp, p-1)
      return true
    end
 * space 

local var_assgn =  
    function(s,p)
      local id = closedVar
      local isReserved = keywords[id]
      if isReserved then
        report_syntax_error(s, p, "cannot assign to reserved word ".."'"..id.."'\n")
        return false 
      else
        --print("var to assgn p="..tostring(p), "var=".."'"..id.."'", "isReserved="..tostring(isReserved))
        ast_vars[id] = true
        return true
      end
    end
 * space 

local var_use = #(ID) * function(s,p) pp = p; return true end * lpeg.C(ID) * 
    function(s,p)
      local id = string.sub(s, pp, p-1)
      local isDefined = ast_vars[id]
      --print("p="..tostring(p), "var=".."'"..id.."'", "isDefined="..tostring(isDefined))
      local msg = ""
      if not isDefined then
        msg = msg .. "variable ".."'"..id.."'".." is not defined"
      end
      local isReserved = keywords[id]
      if isReserved then
         msg = msg .. "\ncould not redefine the reserved word ".."'"..id.."' anyway"
      end
      if (not isDefined) or isReserved then report_syntax_error(s, p, msg.."\n") end
      if not isDefined then
        return false 
      else
        return true
      end
    end
  * space / nodeVar

local OP = P("(") * space      -- open parenthesis      
local CP = P(")") * space      -- close parenthesis
local OB = P("{") * space      -- open braces      
local CB = P("}") * space      -- close braces

-- operators from lowest to highest priority
local opA = lpeg.C(lpeg.S("+-|")) * space  --binary left associative add, subract, concat (for zone)
local opM = lpeg.C(lpeg.S("*/%")) * space  --binary left assocociative multiply, divide, remainder
local opZ = lpeg.C(P(":")) * space    --binary left associative zone
local opE = lpeg.C(lpeg.S("^")) * space    --binary right associative exponent
local opS = lpeg.C(lpeg.S("!")) * space    --unuary suffix left associative factorial
local opP = lpeg.C(lpeg.S("-+")) * space   --unuary right associative, spaces optional           
local opPs= lpeg.C(                        --unuary right associative, spaces mandatory (could be an ID otherwise)
            P"dec"       
                + "inc"
                + "not"
            ) * spaces 

-- "<", ">", ">=", "<=", "==", and "!="
local opR = lpeg.C(P("<=") + ">=" + "~=" + "==" + "<" + ">") * space

--local block_comment = lpeg.V"block_comment"
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

local function calc_maxp(s,p) p_maxmatch = math.max(p_maxmatch, p); return true end

function GRAMMER() end -- for IDE outline view

local grammar = P{"prog",
  prog = space * stmts * -1,
  stmts = stmt * (T";"^1 * stmts)^-1 / nodeSeq,
  block = T"{" * (stmts) * T";"^-1 * T"}",
  stmt = T"(" * T")" -- empty block
      + block 
      + Rw"return" * notty / nodeReturn -- return
      + var_closure * T"=" * var_assgn * notty / nodeAssgn 
      + T"@" * notty / nodePrint -- print (don't make a unary op because print(print( )) is undesirable
                                    -- TODO: capture an optional string to prefix output e.g. "x="
      + space, 
  factor = number + T"(" * notty * T")" + var_use, 
  prefix = lpeg.Ct((opP + opPs)^1  * factor)  / foldPre  -- prefix ops (except 'not' trapped below) trap here before nodeVar sees them 
           + factor,                                     -- but nodeVar traps the others in expressions below
  zone = lpeg.Ct(prefix * (opZ  * prefix)^0 ) / foldBin  * space,
  suffix = lpeg.Ct(zone * opS^0) / foldSuf * space,
  expo = lpeg.Ct(suffix * (opE * suffix)^0) / foldRight  * space,
  term = lpeg.Ct(expo * (opM * expo)^0 ) / foldBin  * space,
  expr = lpeg.Ct(term * (opA  * term)^0 ) / foldBin  * space,
  relation = lpeg.Ct(expr * (opR  * expr)^0) / foldBin  * space, --TODO: create foldRel to do a<b<c mathematically as a<b and b<c (reject malformed cases per cpp2)
  notty =  lpeg.Ct((lpeg.C"not" * spaces)^1 *  relation) / foldPre -- prefix operator 'not' lowest priority for applying to relations
          + relation,
  space = (lpeg.space + block_comment + comment)^0 * calc_maxp,
}
  
-- parse
local function parse(input, options)
  options = options or {}
  options.grammar = options.grammar and options.grammar or grammar --enable experimenting with multiple grammers
  local r = options.grammar:match(input) 
  return r
end

--------------------------------------COMPILER: consume ast and generate code

--------- [op] = instruction code 's

-- left associative binary 
local binOps = {
  ["<"] = "lt", [">"] = "gt", ["<="] = "le", [">="] = "ge", ["=="] = "eq", ["~="] = "ne", 
  ["+"] = "add", ["-"] = "sub", 
  ["*"] = "mul", ["/"] = "div", ["%"] = "rem", ["|"] = "con",
  [":"] = "zone",
}

-- right associative binary
local rightOps = {
  ["^"] = "exp",
}

-- prefix, space optional or mandatory; not is lowest priority, the others prefix priority
local preOps = {
  ["-"] = "neg",
  ["+"] = "pos",
  ["dec"] = "decr",
  ["inc"] = "incr",
  ["not"] = "not",
}

-- repeatable suffix, space optional
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

--------- RECURSIVELY GENERATE CODE FROM AST
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
  options = options or {}
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

function pl(cases, options)
  options = options or {trace = false, grammar = grammar}
  for _, case in ipairs(cases) do   
    io.write("\nCASE--->\n",case)
    
    pp, p_maxmatch, ast_vars = 1, 1, {}
    isSyntaxErrorReported = false 
    local ast = parse(case, options)
    
    -- report any SYNTAX ERROR
    --print("report syntax error??? p_maxmatch="..tostring(p_maxmatch).. " case len="..tostring(string.len(case) ) )
    if p_maxmatch < string.len(case) then 
      if not isSyntaxErrorReported then report_syntax_error(case, p_maxmatch, "", options) end
    else
      -- optionally trace ast
      if options.trace then print("\nast"); print(pt.pt(ast).."\n") end
      local code = compile(ast)
      -- optionally trace code
      if options.trace then print("\ncode"); print(pt.pt(code)) end
      
      -- interpret code
      local stack, mem = {}, {}
      local pgm_result = run(code, stack, mem, options)
      print("\n--->"..tostring(pgm_result))
    end
  end
end

print"\nprimordial pl (PPL) user cases\n"
print("NEW cases")
pl({"x=1+inc 4; return x"},{trace=false, grammar=grammar})
---[=[
pl({"x=1+inc; return x"},{trace=false, grammar=grammar}) --> errors: inc not defined; don't use a reserved word
pl({[[
  {;; }; 
  { ;
    x = 3;;;;
    y; = 1:2; # not an empty statement
  }; 
  return x+y
]]},{trace=false, grammar=grammar})
pl({"x = - inc -4; return x"},{trace=false, grammar=grammar})
pl({  
  [[#{my block comment #{ with a nested block comment # just a comment too
  here }# and rest of comment } # return 1;
]]},{trace=false})

pl({"return y"},{trace=false, grammar=grammar})
pl({"de =dec dec 1; return = de"},{trace=false, grammar=grammar})
pl({[[
    # the next three lines are fine
    x = 1; #{ this, but not the # embedded comments
    is a block comment spanning #{nesting allowed of course} #, multiple lines }#
    @ (x -3<2);   
    @ ((not 3<-1);  # but this line has a syntax error
    return y
]]},{trace=false})

pl({[[
  x=1:3; 
  y=dec 2; 
  z= inc 4; 
  @ (x+7); # should print 8:10
  @ 2<3; 
  @ not (2<3); 
  return -x + y+z
]]},{trace=false, grammar=grammar})
pl({"x = y!; return x"},{trace=false, grammar=grammar}) --> error: y is not a defined variable

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

pl(tests,{grammar=grammar})
--]=]

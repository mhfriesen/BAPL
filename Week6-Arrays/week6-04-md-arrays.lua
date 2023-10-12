local M = {} -- module

local lpeg = require"lpeg"
local pt = require"pt" 

-- some IDE outline views highlight non local functions; in any case CAPS standout; thus the following hack
function FUNCTIONS() end --------------------------------------FUNCTIONS: general

-- take members of a table
--    first n1 members (n2 nil)
--    n1 to n2 members
local function take(t,n1, n2)
  local r = {}
  if not n2 then -- take first n1
    table.move(t, 1, n1, 1, r)
  else -- take n1 to n2
    table.move(t, n1, n2, 1, r)
  end
  return r
end

-- iterate a table by sorted keys
local function kpairs (t, f)
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
-- hydro generator zones; similar to intervals but with subtleties to come
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

local function split(s, sep)
  sep = lpeg.P(sep)
  local elem = lpeg.C((1 - sep)^0)
  local p = lpeg.Ct(elem * (sep * elem)^0)   -- make a table capture
  return lpeg.match(p, s)
end



function FRONT_END() end  --------------------------------------FRONT END: parse input and make ast

--------- GLOBAL TO PARSER VARIABLES 
local syntaxErr = false   -- to supress superfluous reporting
local ast_vars = {}       -- for saving vars by index
local p_maxmatch = 1      -- max position reached by lpeg
local pp = 1              -- for saving lpeg position p
local capturedVar = ""    -- for saving ID for subsequent inspection
local input = ""          -- for syntax error reporting  

--------- PRIMORDIAL LPEG PATTERNS 
lpeg.locale(lpeg) -- adds locale entries
local P = lpeg.P 

-- lpeg debugging matchtime function pattern
local function I(msg)
  return function(s,p) print(msg.." p="..tostring(p)) return true end
end

-- generate lpeg exactly pattern
local function exactly(pat, n) 
  local p = P(0) * pat -- enable p to be a string
  return #(p^n) * p^-n * -p -- check(at least 6 p) * at most 6 p * not p
end

local nl = P("\n")     -- new line
local nnl = P(1) - nl  -- anything except a new line; i.e. not new line equiv to -#(P("\n")) * P(1)

-- SYNTAX ERROR reporting
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

function M.report_syntax_error(case, pmax, msg, options)
  options = options or {trace = false}
  local lines_range = calc_lines_range(case)
  local nLine, range = calc_pmax_line_and_range(pmax, lines_range)
  print("\n\nSYNTAX ERROR (pos "..tostring(pmax-1).." of "..tostring(#case).."):")
  local num = string.format("%03d",nLine)
  io.write(num," ",
    string.sub(case, range[1], range[2]-1), "\n",
    string.rep(" ", 3 + pmax - range[1]), "^\n"   -- put ^ under msg at syntax err position
  )
  io.write(msg.."\n")
  --if options.trace then print("lines_ranges", pt.pt(lines_range)) end
  syntaxErr = true
end

--------- MORE LPEG PATTERNS 
function BLOCK_PATTERNS() end -- for IDE outline 

local comment = "#" * nnl^0

-- matchtime nested block comments

local function nested_block_comments() --close over level
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
      if #s <= p then
        M.report_syntax_error(s, p, "block comment closing '}*' not found\n")
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
  
-- use space pattern to trace how far matching gets since spaces are pervasive
local space = lpeg.V"space" -- i.e. defined in grammar for efficiency            
              
local spaces = lpeg.space^1


--------- AST BUILDING FUCTIONS (called recursively by grammar)
function AST_BUILDING_FUNCS() end -- for IDE outline 

local function nodeSeq(stmt1, stmt2)
  if stmt2 == nil then
    return stmt1
  else
    return {tag = "seq", stmt1 = stmt1, stmt2 = stmt2}
  end
end

-- simple node function generator
local function node (tag, ...)
  local labels = table.pack(...)
  return function (...)
    local params = table.pack(...)
    local t = { tag = tag}
    for i, lab in ipairs(labels) do
      t[lab] = params[i]
    end
    return t
  end
end

-- convert captured lists into a tree, left associatively
local function foldBin(lst)
  local tree = lst[1]
  for i=2, #lst, 2 do
    tree = { tag = "binop", e1 = tree, op = lst[i], e2 = lst[i+1] }
  end
  return tree
end

-- only allow a triplet; syntax error otherwise
-- pick two of three
local function composeRel(lst)
  local tree = { }
  if 5 < #lst then
    if not syntaxErr then M.report_syntax_error(input, p_maxmatch, "mathematical relations: only 2 relations allowed") end
    tree = {tag = "syntaxErr"}
  elseif #lst == 5 then
    local exp1 = {tag= "binop", e1 = lst[1], op = lst[2], e2 = lst[3] }  
    local exp2 = {tag = "binop", e1 = lst[3], op = lst[4], e2 = lst[5] }  
    tree = { tag = "binop", e1 = exp1, op = "and", e2 = exp2 } -- non sc and
  else
    tree = foldBin(lst)
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
  local tree = lst[#lst]
  for i = #lst-1, 1, -1 do
    tree = { tag = "preop", e1 = tree, op = lst[i] }
  end
  return tree
end

function TOKEN_PATTERNS() end -- for IDE outline view

local digits = lpeg.R("09")^1
local hexdigits = lpeg.R("09","AF","af")
local dot = P(".")
local hex = "0" * lpeg.S("xX") * hexdigits^1 
local decimal = digits * dot * digits +
                digits * dot  + 
                dot * digits +
                digits
local scientific = decimal *  lpeg.S("eE") * ((exactly("-",1) * digits) + digits)
local number = lpeg.C(scientific + hex + decimal)  * space 


local underscore = P("_")
local alpha = lpeg.R("az") + lpeg.R("AZ")
local numeric = lpeg.R("09")
local alpha_numeric = alpha + numeric

local identifier =  (underscore + alpha) * (alpha_numeric + underscore)^0
local object = identifier * P(".") * identifier
local ID = identifier + object

-- token pattern factories
local function T(str) return str * space end

-- reserved words
local keywords = {
  ["nil"] = true, 
  ["new"] = true, 
  ["not"] = true, 
  ["inc"] = true, 
  ["dec"] = true, 
  ["return"] = true, 
  ["if"] = true, 
  ["else"] = true, 
  ["elseif"] = true, 
  ["while"] = true, 
  ["and"] = true, 
  ["or"] = true, 
}    

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
            ) * spaces --TODO: * -alpha_numeric ? e.g. test not(1)

-- "<", ">", ">=", "<=", "==", and "!="
local opR = lpeg.C(P("<=") + ">=" + "~=" + "==" + "<" + ">") * space
local opL = lpeg.C(P"and" + "or") * #(-alpha_numeric) * space

local function Rw(word) 
  assert(keywords[word]) -- defensive pgming to ensure we've added it to keywords
  return word * -#(alpha_numeric) * space 
end


function ID_CAPTURES() end -- for IDE outline view
-- mt captures verify ID type (var, indexed, new)
-- catch and report any syntax/semantic errors
-- capture ID for further parsing

local indexed_capture = #(ID * space * T"[") *      -- is indexed var
    function(s,p) pp = p; return true end * -- matchtime save ID position in pp
    lpeg.C(ID) *                            -- capture ID
    function(s,p)                           -- matchtime syntax/semantics checks                                    
      local id = string.sub(s, pp, p - 1)
      local isReserved = keywords[id]
      local isArray = ast_vars[id]
      if isReserved then
        if not syntaxErr then M.report_syntax_error(s, p, "cannot assign to reserved word ".."'"..id.."'\n") end
        return false 
      elseif not isArray then
        if not syntaxErr then M.report_syntax_error(s, p, "array ".."'"..id.."' must be newed first\n") end
        return false
      end
      return true
    end
 * space 
 
local new_capture = #(ID * space * T"="  * T"new") * -- is new array 
    function(s,p) pp = p; return true end * -- matchtime save ID position in pp
    lpeg.C(ID) *                            -- capture ID
    function(s,p)                           -- matchtime syntax/semantic checks and register var                                    
      local id = string.sub(s, pp, p - 1)
      local isReserved = keywords[id]
      if isReserved then
        if not syntaxErr then M.report_syntax_error(s, p, "cannot assign to reserved word ".."'"..id.."'\n") end
        return false 
      else
        ast_vars[id] = true
        return true
      end
    end
    / node("variable", "var") * space * T"=" * T"new"
 
local var_capture = #(ID * space * T"=") *  -- is var 
    function(s,p) pp = p; return true end * -- matchtime save ID position in pp
    lpeg.C(ID) *                            -- capture ID
    function(s,p)                           -- matchtime syntax/semantic checks and register var                                    
      local id = string.sub(s, pp, p - 1)
      local isReserved = keywords[id]
      if isReserved then
        if not syntaxErr then M.report_syntax_error(s, p, "cannot assign to reserved word ".."'"..id.."'\n") end
        return false 
      else
        ast_vars[id] = true
        return true
      end
    end
 / node("variable", "var") * space * T"=" 
 
local nil_capture = #(ID * space * T"=" * T"nil") *  -- is nil 
    function(s,p) pp = p; return true end * -- matchtime save ID position in pp
    lpeg.C(ID) *                            -- capture ID
    function(s,p)                           -- matchtime syntax/semantic checks and register var                                    
      local id = string.sub(s, pp, p - 1)
      local isReserved = keywords[id]
      if isReserved then
        if not syntaxErr then M.report_syntax_error(s, p, "cannot nil a reserved word ".."'"..id.."'\n") end
        return false 
      else
        ast_vars[id]=nil -- GC var
        return true
      end
    end
   * space * T"=" * T"nil"
 
 
function ID_USE() end -- for IDE outline view

local indexed_use = #(ID * space * T"[") * function(s,p) pp = p; return true end * lpeg.C(ID) * 
    function(s,p)
      local id = string.sub(s, pp, p-1)
      local isReserved = keywords[id]
      local isArray = ast_vars[id]
      local msg = ""
      if not isArray then
        msg = msg .. "array '" ..id.. "' does not exist (must be newed first)"
      end
      if isReserved then
         msg = msg .. "\ncould not redefine the reserved word ".."'"..id.."' anyway"
      end
      if not syntaxErr and (not isArray or isReserved) then
        M.report_syntax_error(s, p, msg.."\n") 
      end
      if not isArray then
        return false 
      end
      return true
    end
  * space 
  
local var_use = #(ID) * function(s,p) pp = p; return true end * lpeg.C(ID) *
    function(s,p)
      local id = string.sub(s, pp, p-1)
      local isReserved = keywords[id]
      local isDefined = ast_vars[id]
      local msg = ""
      if not isDefined then
        msg = msg .. "variable '" ..id.. "' does not exist"
      end
      if isReserved then
         msg = msg .. "\ncould not redefine the reserved word ".."'"..id.."' anyway"
      end
      if not syntaxErr and (not isDefined or isReserved) then
        M.report_syntax_error(s, p, msg.."\n") 
      end
      if not isDefined then
        return false 
      else
        return true
      end
    end
  * space 

local function foldNew_assgn(lst) 
  local sizes = { } 
  for i = #lst, 2, -1 do
    sizes[#sizes + 1] = lst[i]
  end
  local tree = {tag = "assgn", lhs =  lst[1], expr = {tag = 'new', sizes = sizes}}
  return tree
end

local function foldIndexed_assgn(lst)
  local lhs = {tag="variable", var = lst[1]} 
  for i = 2, #lst - 1 do
    lhs = {tag = "indexed", array = lhs, index = lst[i] }
  end
  local tree = {tag = "assgn", expr = lst[#lst], lhs = lhs}
  return tree
end

local function var_assgn(ast) 
  local tree = {tag = "assgn", lhs = ast[1], expr = ast[2]}
  return tree
end

local function foldIndexed_use(lst)
  local tree = {tag="variable", var = lst[1]} -- array e.g. {tag = 'variable', var = 'x'} TODO: capture that in ind_capture
  for i = 2, #lst do
    tree = {tag = "indexed", array = tree, index = lst[i] }
  end
  return tree
end



--local block_comment = lpeg.V"block_comment"
local stmt = lpeg.V"stmt"
local stmts = lpeg.V"stmts"
local block = lpeg.V"block"
local nested_elseifs = lpeg.V"nested_elseifs"
local factor = lpeg.V"factor"
local prefix = lpeg.V"prefix"
local zone = lpeg.V"zone"
local suffix = lpeg.V"suffix"
local expo = lpeg.V"expo"
local term = lpeg.V"term"
local expr = lpeg.V"expr"
local relation = lpeg.V"relation"
local notty = lpeg.V"notty"
local shortcct = lpeg.V"shortcct"

local function calc_maxp(s,p) p_maxmatch = math.max(p_maxmatch, p); return true end
    
function GRAMMER() end -- for IDE outline view

local Ct = lpeg.Ct

-- string pattern
local open = lpeg.P('"')
local close = -lpeg.B("\\") * open + open * -1 
local strpat = open * lpeg.C((1 - close)^0) * close * space


local grammar = P{"prog",
  prog = space * stmts * -1,
  stmts = stmt * (T";"^1 * stmts)^-1 / nodeSeq,
  stmt = 
        T"{" * T"}" 
      + block
      + Rw"while" * shortcct * block / node("while_", "cond", "body")   
      + Rw"if" * shortcct * block * nested_elseifs^-1 / node("if_", "cond", "then_", "else_")
      + Rw"return" * shortcct / node("return", "expr") 
      
      -- assignments
      + Ct(new_capture * (T"[" * shortcct * T"]" )^1 ) / foldNew_assgn  
      + Ct(indexed_capture * ( T"[" * shortcct * T"]" )^1 * T"=" * shortcct ) / foldIndexed_assgn   
      + nil_capture -- GC vars and arrays
      + Ct(var_capture * shortcct) / var_assgn
      
      + T"@" * shortcct / node("print","expr") -- printing
      + space, 
  block = T"{" * stmts * T";"^-1 * T"}",
  nested_elseifs = Rw"else" * block + Rw"elseif" * shortcct * block *  nested_elseifs^-1
                  / node("if_", "cond", "then_", "else_"),
  factor = 
        number  / tonumber / node("number","val")
      + strpat / node("string", "val") -- glimmerings of string type 
      + T"(" * shortcct * T")" 
      + Ct(indexed_use * (T"[" * shortcct * T"]")^0) / foldIndexed_use 
      + T"nil"
      + var_use / node("variable", "var"),
  prefix = Ct((opP + opPs)^1  * factor )  / foldPre + factor,                                  
  zone = Ct(prefix * (opZ  * prefix)^1 ) / foldBin  * space + prefix,
  suffix = Ct(zone * opS^1) / foldSuf * space + zone,
  expo = Ct(suffix * (opE * suffix)^1 ) / foldRight  * space + suffix,
  term = Ct(expo * (opM * expo)^1 ) / foldBin  * space + expo,
  expr = Ct(term * (opA  * term)^1 ) / foldBin  * space + term,
  relation = Ct(expr * (opR  * expr)^1 ) / composeRel  * space + expr,   
  notty =  Ct(opPs^1 * relation) / foldPre  + relation,
  shortcct = lpeg.Ct(notty * (opL * notty)^1 ) / foldBin  * space + notty, 
  space = (lpeg.space + block_comment + comment)^0 * calc_maxp,
}


-- parse

function M.parse(case, options)
  options = options or {}
  
  -- reset global to parser variables that don't reset themselves internally (.e.g. pp, capturedVar)
  syntaxErr = false
  ast_vars = {}
  p_maxmatch = 1
  input = case  
  
  local ast = grammar:match(input) 
  return ast, p_maxmatch, syntaxErr
end

--------------------------------------COMPILER: consume ast and generate code
local Compiler = {code = {}, vars = {}, nvars = 0}

--------- [op] = instruction codes

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


--------- RECURSIVELY GENERATE CODE FROM AST
function Compiler:addCode(op)
  local code = self.code
  code[#code + 1] = op
end

-- convert variable names to an index into list they are stored in (for direct lookup)
function Compiler:var2num (id)
  local num = self.vars[id]
  if not num then
    num = self.nvars + 1
    self.nvars = num
    self.vars[id] = num
  end
  return num
end

function Compiler:currentPosition()
  return #self.code
end

function Compiler:codeJmp(op)
  self:addCode(op)
  self:addCode(self:currentPosition()+1) -- to be updated later relative to the pos coded here
  return self:currentPosition()
end

function Compiler:codeJmpB(op, ilabel)
  self:addCode(op)
  self:addCode(ilabel - self:currentPosition() - 1) -- relative jmp
end

function Compiler:fixJmp2here(jmp)
  self.code[jmp] = self:currentPosition() - self.code[jmp] -- relative jump
end

function Compiler:codeExp(ast)
  if ast.tag == "number" then
    self:addCode("push")
    self:addCode(ast.val)
  
  elseif ast.tag == "string" then
    self:addCode("push")
    self:addCode(ast.val)
 
  elseif ast.tag == "binop" then
 
    -- add checks for valid binops here, but that would require tagging expressions with types as they are built
    
    if ast.op == "and" then
      -- code short cct and; i.e. if e1=true leave e2 on stack; else leave e1 result(i.e. false on stack and skip e2) 
      self:codeExp(ast.e1)    
      local jmp = self:codeJmp("jmpA")     -- (jmpS) leaves cond on stack
      self:codeExp(ast.e2) 
      self:fixJmp2here(jmp)  
    elseif ast.op == "or" then
      -- code short cct or; i.e. if e1=true leave e1 on stack (and skip e2); else leave e2 result on stack 
      self:codeExp(ast.e1)
      local jmp = self:codeJmp("jmpO")     
      self:codeExp(ast.e2) 
      self:fixJmp2here(jmp)                
    else
      self:codeExp(ast.e1)
      self:codeExp(ast.e2)
      self:addCode(binOps[ast.op])
    end
  
  elseif ast.tag == "rightop" then
    self:codeExp(ast.e2)
    self:codeExp(ast.e1)
    self:addCode(rightOps[ast.op])
  
  elseif ast.tag == "preop" then
    self:codeExp(ast.e1)
    self:addCode(preOps[ast.op])
  
  elseif ast.tag == "sufop" then
    self:codeExp(ast.e1)
    self:addCode(sufOps[ast.op])
  
  elseif ast.tag == "variable" then
    self:addCode("load")
    self:addCode(self:var2num(ast.var))
  
  elseif ast.tag == "new" then  -- vm to put in mem associated with 
      for i = 1, #ast.sizes do
        self:codeExp(ast.sizes[i])
      end
      self:codeExp({tag="number", val = #ast.sizes})
      self:addCode("newarray")
    
  elseif ast.tag == "indexed" then -- vm mem[array][index] with array, index on stack
      self:codeExp(ast.array)
      self:codeExp(ast.index)
      self:addCode("getarray")
  
  elseif ast.tag == "syntaxErr" then
  
  else 
    print("codeExp invalid ast?:",pt.pt(ast))
    error("CALP: invalid tree ast.tag="..ast.tag)
  end
end

function Compiler:codeAssgn(ast)
  local lhs = ast.lhs
  if lhs.tag == "variable" then
      self:codeExp(ast.expr)
      self:addCode("store")
      self:addCode(self:var2num(lhs.var))
  elseif lhs.tag == "indexed" then
    self:codeExp(lhs.array)
    self:codeExp(lhs.index)
    self:codeExp(ast.expr)
    self:addCode("setarray")        -- vm to mem[var2num][index] = expr from stack expr, index, var2num
  else 
    print("unknown codeAssgn tag ast=",pt.pt(ast))
    error("unknown codeAssgn tag")
  end
end
  

-- generate code from ast statements
function Compiler:codeStmt(ast)
  if ast == nil or ast.tag == nil  then
    -- empty stmt, or syntaxErr do nothing
  
  elseif ast.tag == "assgn" then
    self:codeAssgn(ast)
  
  elseif ast.tag == "seq" then
    self:codeStmt(ast.stmt1)
    self:codeStmt(ast.stmt2)
  
  elseif ast.tag == "while_" then            
    local ilabel = self:currentPosition()
    self:codeExp(ast.cond)
    local jmp = self:codeJmp("jmpZ")    -- (jmpZ) conditional jump
    self:codeStmt(ast.body)
    self:codeJmpB("jmpZ", ilabel)       -- reuse jmpZ VM instruction but give a negative value
    self:fixJmp2here(jmp)
    
  elseif ast.tag == "if_" then
    self:codeExp(ast.cond)
    local jmp = self:codeJmp("jmpZ")     
    self:codeStmt(ast.then_) 
    if not ast.else_ then
      self:fixJmp2here(jmp)                
    else                                -- if then_ block executes, we need to skip the else_ block 
      local jmp2 = self:codeJmp("jmp")  -- (jmp) unconditional jump
      self:fixJmp2here(jmp)             -- fix conditional jump to skip uncondional jump
      self:codeStmt(ast.else_)
      self:fixJmp2here(jmp2)
    end
  
  elseif ast.tag == "return" then
    self:codeExp(ast.expr)
    self:addCode("return")
  
  elseif ast.tag == "print" then
    self:codeExp(ast.expr)
    self:addCode("print")
  
  elseif ast.tag == "syntaxErr" then
  
  else 
    print("codeStmt invalid ast?:",pt.pt(ast))
    error("invalid tree ast.tag="..ast.tag)
  end
end

-- generate code from abstract syntax tree
function M.compile(ast)
 --print("in compile ast:",pt.pt(ast))
 Compiler.code, Compiler.vars, Compiler.nvars = {}, {}, 0
 Compiler:codeStmt(ast)

  -- make all programs, aka code generated, end with a return stmt
  Compiler:addCode("push")
  Compiler:addCode(0)
  Compiler:addCode("return")
  return Compiler.code
end

--------------------------------------VIRTUAL MACHINE: execute generated code
  local function ps(stack, top, mem) -- print stack
    local s = " ["
    for i=1,top do
      s = s .. tostring(stack[i]) .. " "
    end
    s = s .. "] mem "
    for i,v in ipairs(mem) do
      s = s .. tostring(i) .. "=" .. tostring(v) .. " "
    end
    return s 
  end
  
  local function create_new_array(n, dims)
  if n == #dims then
      return {size=dims[n]}
  else
    local array = {size=dims[n]}
    for i = 1, dims[n] do
      array[#array + 1] = create_new_array(n+1,dims)
    end
    return array
  end
end

local function run(code, stack, mem, options)
  options = options or {}
  prnt = options.trace and io.write or function() end -- optionally trace interpretation
  prnt("\ninterpret\n")
  local pc = 1
  local top = 0
  local flag_print = false -- to newline first print instruction
  
  while true do 
    prnt(tonumber(pc).. " "..code[pc], ps(stack, top, mem))
    if code[pc] == "return" then 
      return stack[top]
    elseif code[pc] == "print" then 
      if not flag_print then -- newline first print instruction
        print()
        flag_print = true
      end
      io.write("printing: ")
      if type(stack[top]) == "table" then
        print(pt.pt(stack[top]))
      else
        print(stack[top])
      end
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
    elseif code[pc] == "newarray" then -- {size=dims[1], {size=dims[2]... dims[1]
      local n = stack[top] -- n dims
      local dims = {}
      for i = 1, n do
        dims[#dims + 1] = stack[top - i]
      end
      local newedArray = create_new_array(1, dims)
      top = top - n
      stack[top] = newedArray
    elseif code[pc] == "getarray" then -- vm mem[array][index] with array, index on stack
      local index = stack[top]
      local array = stack[top - 1]
      if not (1 <= index and index <= array.size) then
        print("RUNTIME ERROR: getarray out of bounds: index = " .. tostring(index) .. ",  bounds = 1 to " .. tostring(array.size))
        return 0
      else
        stack[top - 1] = array[index]
      end
      top = top -1 
    elseif code[pc] == "setarray" then -- mem[var2num][index] = expr from stack expr, index, table
      local value = stack[top]
      local index = stack[top - 1]
      local array = stack[top - 2]
      top = top - 3
      if array.size < index then
        print("RUNTIME ERROR: setarray out of bounds: index = " .. tostring(index) .. ",  bounds = 1 to " .. tostring(array.size))
        return 0
      else
        array[index] = value
      end
    elseif code[pc] == "jmp" then 
      pc = pc + code[pc] -- unconditional relative jump
    elseif code[pc] == "jmpZ" then 
      pc = pc + 1
      if stack[top] == 0 or stack[top] == nil then -- conditional
        pc = pc + code[pc] -- relative jump
      end
      top = top  -1
    elseif code[pc] == "jmpA" then  -- and short cct jump leaves cond on stack
      pc = pc + 1
      if stack[top] == 0 or stack[top] == nil then -- conditional
        pc = pc + code[pc] -- relative jump
      end
    elseif code[pc] == "jmpO" then  -- or short cct not jump leaves cond on stack
      pc = pc + 1
      if not ( stack[top] == 0 or stack[top] == nil) then -- conditional
        pc = pc + code[pc] -- relative jump
      end
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
    elseif code[pc] == "and" then 
      stack[top-1] = stack[top-1] and stack[top] and 1 or 0
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
    else 
      error("unkown instruction "..code[pc])
    end
    prnt(ps(stack, top, mem)); prnt("\n")
    pc = pc + 1
  end
end

--------------------------------------USER: program in new language, run compiler on it, interpret it
local function pl(cases, options)
  options = options or {trace = false}
  for i, case in ipairs(cases) do  
    local num = string.format("%03d",i)
    io.write("\nPROGRAM ----------------------------- ", num, " ------------------------------\n")
    local lines = split(case,"\n")
    for i, line in ipairs(lines) do
        local num = string.format("%03d",i)
        io.write(num, " ", line, "\n")
    end
        
    ast, p_maxmatch, syntaxErr = M.parse(case, options)
    
    -- report any SYNTAX ERROR
    if p_maxmatch < #case then 
      if not syntaxErr then M.report_syntax_error(case, p_maxmatch, "", options) end
    elseif not syntaxErr then
      -- optionally trace ast
      if options.trace then print("\nast"); print(pt.pt(ast).."\n") end
      local code = M.compile(ast)
      -- optionally trace code
      if options.trace then print("\ncode"); print(pt.pt(code)) end
      
      -- interpret code
      local stack, mem = {}, {}
      local ok, pgm_result = pcall(function ( )
          return  run(code, stack, mem, options)
      end)
      if ok then
        io.write("\n--->")
        if type(pgm_result) == "table" then
          io.write(pt.pt(pgm_result))
        else
          io.write(tostring(pgm_result))
        end
        io.write("\n")
      else
        io.write("RUNTIME ERROR: ")
        local pos_last_colon = #pgm_result - string.find(string.reverse(pgm_result),":")
        print(string.sub(pgm_result,pos_last_colon+3,#pgm_result))
      end
    end
  end
end

print"\nArrays Now There, Strings Yes pl -- aka ANTSY\n"
print("NEW cases")

function CASES() end -- for IDE outline view
cases = {
[[
a = new[3];
a[1] = a; #--- arrays that like themselves
@ a]]
,
[[
a = new[3];
b = new[2];
a[2] = b; #--- arrays that like other arrays
@ a]]
,
[[
a = new[4][3];
a[2][2] = 5;
a = nil;  #--- garbage collection
a[2][2] = 6;
@ a]]
,
[[
x = 1;
@ x;
x = nil;
@ x]]
,
[[
a = new[4][3];
b[3][5] = 9999;
@ b]]
,
[[
a = new[4][3];
a[3][2] = 9999;
@ a[3][2];
a[2][3] = 99;
return a[2][3] ]]
,
[[
a = new[4][3];
a[3][3] = 9;
@ "oooo... string type" | " so we can say we got to line " | (2 + 1);
a[3][5] = 9999;
@ "wish we got this far"]]
,
[[@ "mind those runtime errors" | 2 + 1]]
,
[[
a = new[4][3];
@ "got here";
@ a[3][5];
@ "but not here, so go figure"]]
,
[[
s= 3;
a = new[(s+1)/2+1][s-1];
a[2][1] = 999999;
return a]],
[[
s = 10;
a = new[s];
i = 1;
@ "Let's have fun for a while!";
while i <= s {
  a[i] = i!;
  @ i | "! = " | a[i];
  i = i + 1
};
return a ]],
[[
a = new[10];
a[9] = 4;
a[10] = a[9];
@ a[10];
@ a;
return a]],
"x=1+inc[3]; return x",
"x=1+inc 4; return x",
[[return a ]],
[[return "abc" ]],
"return not 1 < 8 < 4",
"return not 1 < 8 < 4!",
"return 2-2 and 5-3 or 4+1", 
"return not 1 or 3 and 5",
"return 1! and 0 or 7",
[[
if 0 {
  return 5
} elseif 0 {
  return 7
} elseif 0 {
  return 9
} else {
  return 999
}
]],
[[
if 2 < 1 {
  return 5
} elseif 0 {
  return 7
} elseif 0 {
  return 9
} else {
  if 0 {return 99} elseif not 3 < 2 {return 999}
}
]],
[[
if 2 < 1 {
  return 5
} elseif 0 {
  return 7
} elseif 0 {
  return 9
} else {
  if 0 {return 99} elseif not (1 < 9 < 3) {return 999}
}
]],
[[
if 2 < 1 {
  return 5
} elseif 0 {
  return 7
} elseif 0 {
  return 9
} else {
  if 0 {return 99} elseif not 1 < 9 < 3 {return 999}
}
]],
[[
if 0 {
  return 5
} elseif 1 < 3 < 5 {
  return 7
} elseif 0 {
  return 99
}             # no else
]],
[[if 0 {
  return 5
} elseif 0 {
    return 7
} elseif 0 {
    return 99
}             # no else
]],
[[
if 0 {
  return 5
} else {
  if 0 {
    return 7
  } else {
    return 99
  }
}
]],
[[
x = 1 < 5 < 4;  # oooo... mathematical relations
y = 1 < 4 <= 4;
return x:y  # zone type used for multiple return values
]],
[[
x = 5;
return 1 < x < 4 < 7]]
,
"return x = 1 < 5 < 4",
[[
  {;; }; 
  { ;         # empty statements galore!!
    x = 3;;;;
    y; = 1:2; # but, this is not an empty statement
  }; 
  return x+y
]],
"x = - inc -4; return x",
[[#{my block comment #{ with a nested block comment # just a comment too
  here }# and rest of comment } # return 1;
]],
"return y",
"de =dec dec 1; return de #----- unary ops",
"de =-dec dec 1; return de",
"de =dec -dec 1; return de",
"de =dec - dec 1; return de",
"de =dec dec 1; return = de" ,
[[
    # the next three lines are fine
    x = 1; #{ this, but not the # embedded comments
    is a block comment spanning #{nesting allowed of course} #, multiple lines }#
    @ (x -3<2);   
    @ ((not 3<-1);  # but this line has a syntax error
    return y
]],

[[
  x=1:3; 
  y=dec 2; 
  z= inc 4; 
  @ (x+7); # should print 8:10
  @ 2<3; 
  @ not (2<3); 
  return -x + y+z
]],

"x = y!; return x"
}

pl(take(cases,1, 12),{trace=false})

--[=[
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

pl(tests,{trace=false})
--]=]
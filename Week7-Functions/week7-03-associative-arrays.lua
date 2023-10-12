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
  f = f or function(a, b) return a < b end
  for n in pairs(t) do table.insert(a, n) end
  table.sort(a, f)
  local i = 0      -- iterator variable
  local iter = function ()   -- iterator function
    i = i + 1
    if a[i] == nil then 
      return nil
    else 
      return a[i], t[a[i]]
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
local ast_vars = {}       -- vars by key
local ast_funcs = {}      -- funcs by key
local fwd_funcs = {}      -- fwd decl'd funcs by key
local fwd_funcs_body = {}      -- fwd decl'd funcs by key
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

-- transform string with interpolation expressions 
-- e.g. "A {} with {} interpolations." <- "string" <- 1 + 1 ---> A string with 2 interpolations."
--
local function foldInterp(lst)-- string with optional expressions embedded, optional external expressions
  -- lower ast to a simple concat binOp and call foldBin on it
  --print("\nlst=", pt.pt(lst))
  local s = lst[1].val -- string to interpolate with remaining expressions
  --print("s=", s)
  
  -- captures to match with expressions
  local NO = (1-lpeg.P"{}")^0
  --local NC = (1-lpeg.P"}")^0
  --local captures = lpeg.Ct((NO * lpeg.P"{" * lpeg.C(NC) * lpeg.P"}")^0)
  local captures = lpeg.Ct((NO * lpeg.C(lpeg.P"{}") )^0 )
  captures = captures:match(s)
  --print("captures", pt.pt(captures))
  
  -- just return the string if it has no captures; any exprs get ignored
  if #captures == 0 then
    return {tag = "string", val = s}
  end
  
  -- expressions to interpolate
  local strings = lpeg.Ct((lpeg.C(NO) * P"{}" * lpeg.C(NO))^0)
  --local strings = lpeg.Ct((lpeg.C(NO) * lpeg.P"{" * NC * lpeg.P"}" * lpeg.C(NO) )^0)
  local exprs = {} 
  for i = 3, #lst, 2 do
    exprs[#exprs + 1] = lst[i]
  end
  
  -- pad exprs
  if #exprs < #captures then
    for i = 1, #captures - #exprs do
      exprs[#exprs + 1] = exprs[#exprs]
    end
  end
  --print("exprs", pt.pt(exprs))

  -- strings between the captures for concatenating with exprs 
  strings = strings:match(s)
  --print("\nstrings", pt.pt(strings))
  
  -- ingore surplus exprs
  if  #captures < #exprs then
    for i = 1, #exprs - #captures  do
      table.remove(exprs)
    end
  end
  --print("exprs", pt.pt(exprs))
  

  s = {}
  for i = 2, #strings, 2 do
    if strings[i-1] ~='' then 
      s[#s+1] = {tag="string", val=strings[i-1]}
      s[#s+1] = "|"
    end
    s[#s+1] = exprs[i/2] 
    s[#s+1] = "|"
    s[#s+1] = {tag="string", val=strings[i]}
    s[#s+1] = "|"
  end
  table.remove(s,#s)
  --print("\ns", pt.pt(s))
  return foldBin(s)
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
  ["function"] = true, 
  ["unless"] = true, 
}    

local OP = P("(") * space      -- open parenthesis      
local CP = P(")") * space      -- close parenthesis
local OB = P("{") * space      -- open braces      
local CB = P("}") * space      -- close braces

-- operators from lowest to highest priority
local opA = lpeg.C(
            lpeg.S("+-|") * -#(lpeg.B("<")) * -#(lpeg.P(">")) -- enable <- <| <+ -> |> +> operators
            )
            * space  --binary left associative add, subract, concat (for zone)
local opI = lpeg.C(lpeg.P("<-")) * space   --binary left associative string interpolation
local opM = lpeg.C(lpeg.S("*/%")) * space  --binary left assocociative multiply, divide, remainder
local opZ = lpeg.C(P(":")) * space         --binary left associative zone
local opE = lpeg.C(lpeg.S("^")) * space    --binary right associative exponent
local opS = lpeg.C(lpeg.S("!")) * space    --unuary suffix left associative factorial
local opP = lpeg.C(lpeg.S("-+")) * space   --unuary right associative, spaces optional           
local opPs= lpeg.C(                        --unuary right associative, spaces mandatory (could be an ID otherwise)
            P"dec"       
                + "inc"
                + "not"
            ) * spaces --TODO: * -alpha_numeric ? e.g. test not(1)

-- "<", ">", ">=", "<=", "==", and "!="
local opR = lpeg.C(
                P"<="
                + ">=" 
                + "~=" 
                + "==" 
                + "<" * -#(lpeg.S("+-|"))         -- enable <- <| <+ operators
                + ">" * -#(lpeg.B(lpeg.S("+-|"))) -- enable -> |> +> operators
              ) * space

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
      elseif ast_funcs[id] then
        if not syntaxErr then M.report_syntax_error(s, p, "cannot assign to a function ".."'"..id.."'\n") end
        return false 
      else
        ast_vars[id] = true
        return true
      end
    end
 / node("variable", "var") * space * T"=" 
 
 
local multivar_capture = #(ID * space * (T"," * ID * space)^1 * T"=") *  -- is multivar 
    function(s,p)
      local vars = {}
      local p_eq = string.find(string.sub(s,p),"=") 
      local varstr = string.sub(s, p, p + p_eq - 1)
      local sp = lpeg.S(" \n\t")^0
      local pat = lpeg.Ct(lpeg.C(ID) * sp * ("," * sp * lpeg.C(ID) * sp)^1)
      local ids = pat:match(varstr)
      --print("ids", pt.pt(ids))
      for _,id in ipairs(ids) do
        local isReserved = keywords[id]
        if isReserved then
          local p_res = string.find(varstr, id)
          if not syntaxErr then M.report_syntax_error(s, p + p_res, "cannot assign to reserved word ".."'"..id.."'\n") end
          return false 
        else
          ast_vars[id] = true
        end
      end
      return true
    end
     
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
 
 
function ID_USES() end -- for IDE outline view

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
      if ast_funcs[id] then
        msg =  "It looks like you tried to call a defined func without using braces."
        if not syntaxErr  then
          M.report_syntax_error(s, p, msg.."\n") 
        end
      else
        if not isDefined then
          msg = msg .. "variable '" ..id.. "' does not exist"
          if fwd_funcs[id] then
            msg = msg .. "\nyou may have intended to call the function ".. id
          end
        end
        if isReserved then
           msg = msg .. "\ncould not redefine the reserved word ".."'"..id.."' anyway"
        end
        if not syntaxErr and (not isDefined or isReserved) then
          M.report_syntax_error(s, p, msg.."\n") 
        end
      end
      if not isDefined then
        return false 
      else
        return true
      end
    end
  * space 

local function foldIndexed_use(lst)
  local tree = {tag="variable", var = lst[1]} -- array e.g. {tag = 'variable', var = 'x'} TODO: capture that in ind_capture
  for i = 2, #lst do
    tree = {tag = "indexed", array = tree, index = lst[i] }
  end
  return tree
end

function ID_ASSGNS() end -- for IDE outline view

local function foldNew_assgn(lst) 
  --print("foldNew_assgn lst", pt.pt(lst))
    -- distinquish associative arrays
  if lst[#lst] == "assoc" then
    -- create an associative arrary
    local tree = {tag = "assgn", lhs =  lst[1], expr = {tag = 'new', assoc = {tag = "number", val = #lst-1} }}
    return tree
  else
    local sizes = { } 
    for i = #lst, 2, -1 do
      sizes[#sizes + 1] = lst[i]
    end
    local tree = {tag = "assgn", lhs =  lst[1], expr = {tag = 'new', sizes = sizes}}
    return tree
  end
end

local function foldIndexed_assgn(lst) --array, i1, i2, ...iN, expr
  --print("foldIndexed_assgn lst", pt.pt(lst))
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

local function multivar_assgn(lst) --used for destructuring arrays
  -- for associative arrays the index number will correspond to the i'th iterated key
  
  -- lower to a sequence of indexed assignments
  --print("multivar_assgn lst=", pt.pt(lst))
  local array = lst[#lst].var
  --print("\narray=", array)
  local tree = nodeSeq {tag = "assgn", lhs = {tag = "variable", var = lst[1]}, expr = foldIndexed_use({array, {tag = "number", val = 1}})  }
  
  for i = 2 , #lst-1 do
    tree = nodeSeq( tree, {tag = "assgn", lhs = {tag = "variable", var = lst[i]}, expr = foldIndexed_use({array, {tag = "number", val = i}}) } )
  end
  --print("\nseq=", pt.pt(tree))
  return tree
end

local function nodeFunction(name, body)
  --print("-----name ", name, "'"..tostring(body).."'")
  --print("ast_funcs", pt.pt(ast_funcs))
  --print("fwd_funcs=", pt.pt(fwd_funcs))
  if body=="" then 
    fwd_funcs_body[name] = {}
  else
    fwd_funcs_body[name] = body
  end
  --print("fwd_funcs_body=", pt.pt(fwd_funcs_body))
  local tree = {tag = "function", name = name, body = fwd_funcs_body[name]}
  return tree
end

local function isempty(t)
  for k,_ in pairs(t) do
    return false
  end
  return true
end

local function checkbodies(lst)
  --print("lst", pt.pt(lst), p_maxmatch)
  local funcs = {}
  for i, func in ipairs(lst) do
    funcs[func.name] = func.body
  end
  --print("funcs", pt.pt(funcs))
  for i, func in ipairs(lst) do
    if not funcs[func.name] then
       if not syntaxErr then 
         M.report_syntax_error(input, p_maxmatch-1, "function '" .. func.name .. "' has only been fwd declared\n")      
         return false
      end
    end
  end
  return lst
end

--local block_comment = lpeg.V"block_comment"
local funcDecl = lpeg.V"funcDecl"
local call = lpeg.V"call"
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
local interp = lpeg.V"interp"
local shortcct = lpeg.V"shortcct"

-- captures
local decl_capture = lpeg.V"decl_capture"
local func_capture = lpeg.V"func_capture"
local call_capture = lpeg.V"call_capture"
--TODO: consider adding all the other captures and uses into the grammar for max efficiency

local function calc_maxp(s,p) p_maxmatch = math.max(p_maxmatch, p); return true end
    
function GRAMMER() end -- for IDE outline view

local Ct = lpeg.Ct

-- string pattern
local open = lpeg.P('"')
local close = -lpeg.B("\\") * open + open * -1 
local strpat = open * lpeg.C((1 - close)^0) * close * space


local grammar = P{"prog",
  prog = space * Ct( funcDecl^1 ) / checkbodies * -1,
  
  funcDecl = Rw"function" * func_capture * (block + T";")  / nodeFunction,
  call = call_capture / node("call", "fname"),
  
  stmts = stmt * (T";"^1 * stmts)^-1 / nodeSeq,
  
  stmt = 
        T"{" * T"}" -- empty blocks allowed
      + block
      + Rw"while" * interp * block / node("while_", "cond", "body")   
      + Rw"unless" * interp * block / node("unless", "cond", "body")   
      + Rw"if" * interp * block * nested_elseifs^-1 / node("if_", "cond", "then_", "else_")
      + Rw"return" * interp / node("return", "expr") 
      + call
      
      -- assignments
      + Ct(new_capture * (T"[" * (interp + lpeg.Cc("assoc") ) * T"]")^1) / foldNew_assgn  
      + Ct(indexed_capture * ( T"[" * interp * T"]" )^1 * T"=" * interp ) / foldIndexed_assgn 
      + nil_capture -- GC vars and arrays
      + Ct(var_capture * interp) / var_assgn
      + multivar_capture * Ct(lpeg.C(ID) * (T"," * lpeg.C(ID) * space)^1 * T"="  * interp)
        / multivar_assgn
      
      + T"@" * interp / node("print","expr") -- printing
      + space, 
      
  block = T"{" * stmts * T";"^-1 * T"}" / node("block", "body"), -- extra semicolons allowed
  
  nested_elseifs = Rw"else" * block + Rw"elseif" * interp * block *  nested_elseifs^-1
                  / node("if_", "cond", "then_", "else_"),
                  
  factor = 
        number  / tonumber / node("number","val")
      + strpat / node("string", "val") -- glimmerings of string type 
      + T"(" * interp * T")" 
      + Ct(indexed_use * (T"[" * interp * T"]")^0) / foldIndexed_use 
      + T"nil"
      + call
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
  interp = Ct(shortcct * (opI  * shortcct)^1 ) / foldInterp  * space + shortcct,
  space = (lpeg.space + block_comment + comment)^0 * calc_maxp,
  
  func_capture = #(ID * space * T"(") *     -- is name 
    function(s,p) pp = p; return true end * -- matchtime save ID position in pp
    lpeg.C(ID) *                            -- capture func name
    function(s,p)                           -- matchtime syntax/semantic checks and register var                                    
      local id = string.sub(s, pp, p - 1)
      local isReserved = keywords[id]
      if isReserved then
        if not syntaxErr then M.report_syntax_error(s, p, "cannot use reserved word ".."'"..id.."' as a function name\n") end
        return false 
      else
        fwd_funcs[id] = true
        return true
      end
    end
  * space * T"(" * T")",

  call_capture = #(ID * space * T"(") *     -- is name 
    function(s,p) pp = p; return true end * -- matchtime save ID position in pp
    lpeg.C(ID) *                            -- capture ID aka name
    function(s,p)                           -- matchtime syntax/semantic checks and register var                                    
      local id = string.sub(s, pp, p - 1)
      local isReserved = keywords[id]
      if isReserved then
        if not syntaxErr then M.report_syntax_error(s, p, "cannot use reserved word ".."'"..id.."' as a function call\n") end
        return false 
      elseif not fwd_funcs[id] then
        if not syntaxErr then M.report_syntax_error(s, p, "function '"..id.."' hasn't been defined\n") end
        return false 
      else
        return true
      end
    end
  * space * T"(" * T")"
}


-- parse

function M.parse(case, options)
  options = options or {}
  
  -- reset global to parser variables that don't reset themselves internally (.e.g. pp, capturedVar)
  syntaxErr = false
  ast_vars = {}
  ast_funcs = {}
  fwd_funcs = {}
  fwd_funcs_body = {}
  p_maxmatch = 1
  input = case  
  
  local ast = grammar:match(input) 
  return ast, p_maxmatch, syntaxErr
end

function COMPILER() end ----------------------------------------COMPILER: consume ast and generate code

local Compiler = {funcs = {}, vars = {}, nvars = 0}

--------- [op] = instruction codes

-- left associative binary 
local binOps = {
  ["<"] = "lt", [">"] = "gt", ["<="] = "le", [">="] = "ge", ["=="] = "eq", ["~="] = "ne", 
  ["+"] = "add", ["-"] = "sub", 
  ["*"] = "mul", ["/"] = "div", ["%"] = "rem", ["|"] = "con",
  [":"] = "zone",
  ["<-"] = "interp",
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

function Compiler:currentPC()
  return #self.code
end

function Compiler:codeJmp(op)
  self:addCode(op)
  self:addCode(self:currentPC()+1) -- to be updated later relative to the pos coded here
  return self:currentPC()
end

function Compiler:codeJmpB(op, iPC)
  self:addCode(op)
  self:addCode(iPC - self:currentPC() - 1) -- relative jmp
end

function Compiler:fixJmp2here(jmp)
  self.code[jmp] = self:currentPC() - self.code[jmp] -- relative jump
end

function Compiler:codeCall(ast)
  local func = self.funcs[ast.fname]
  if not func then
    error("undefined function " .. ast.fname)
  end
  self:addCode("call")
  self:addCode(func.code)
end

function Compiler:codeExp(ast)
  if ast.tag == "number" then
    self:addCode("push")
    self:addCode(ast.val)
    
  elseif ast.tag == "call" then
    self:codeCall(ast)
  
  elseif ast.tag == "string" then
    self:addCode("push")
    self:addCode(ast.val)
 
  elseif ast.tag == "binop" then
 
    -- add checks for valid binops here, but that would require tagging expressions with types as they are built
    
    if ast.op == "and" then
      -- code short cct and; i.e. if e1=true leave e2 on stack; else leave e1 result(i.e. false on stack and skip e2) 
      self:codeExp(ast.e1)    
      local jmp = self:codeJmp("jmpA")     -- (jmpS) leaves cond on stack
      self:addCode("pop") -- if we do e2 it means we don't need the cond so pop it
      self:addCode(1)
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
  
    -- distinguish between array and an associated array 
    if ast.sizes then
      for i = 1, #ast.sizes do
        self:codeExp(ast.sizes[i])
      end
      self:codeExp({tag="number", val = #ast.sizes})
      self:addCode("newarray")
    else
      -- code associative array
      self:codeExp({tag="number", val = 1})
      self:addCode("newassoc")
    end
    
  elseif ast.tag == "indexed" then -- vm mem[array][index] with array, index on stack
      self:codeExp(ast.array)
      self:codeExp(ast.index)
      self:addCode("getarray") -- at truntime examine mem[array].size to distinquish associative arrays and index accordingly
  
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

function Compiler:codeBlock(ast)
  self:codeStmt(ast.body)
end


-- generate code from ast statements
function Compiler:codeStmt(ast)
  if ast == nil or ast.tag == nil  then
    -- empty stmt, or syntaxErr do nothing
  
  elseif ast.tag == "assgn" then
    self:codeAssgn(ast)
  
  elseif ast.tag == "call" then
    self:codeCall(ast)
    self:addCode("pop")
    self:addCode(1)
  
  elseif ast.tag == "block" then
    self:codeBlock(ast)

  
  elseif ast.tag == "seq" then
    self:codeStmt(ast.stmt1)
    self:codeStmt(ast.stmt2)
  
  elseif ast.tag == "while_" then            
    local iPC = self:currentPC()
    self:codeExp(ast.cond)
    local jmp = self:codeJmp("jmpZ") -- (jmpZ) conditionally jump past body code and jmp
    self:codeStmt(ast.body)
    self:codeJmpB("jmp", iPC + 1)        -- unconditionally jump back to top of loop (i.e. to start of ast.cond to execute it again)
    self:fixJmp2here(jmp)
    
  elseif ast.tag == "unless" then            
    local iPC = self:currentPC()
    self:codeExp(ast.cond)
    self:addCode("not") -- negate cond
    local jmp = self:codeJmp("jmpZ")    -- (jmpZ) conditional jump
    self:codeStmt(ast.body)
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

function Compiler:codeFunction(ast)
  local code = {}
  -- if a func exists update its code 
  if self.funcs[ast.name] then
    -- remove previous code
    for k in pairs (self.funcs[ast.name].code) do 
      self.funcs[ast.name].code[k] = nil 
    end
    self.code = self.funcs[ast.name].code
  else
      self.funcs[ast.name] = {code = code} 
      self.code = code
  end
  self:codeStmt(ast.body) 
  --print("compiler funcs=====", pt.pt(self.funcs))
  -- make all functions, aka code generated, end with a return stmt
  self:addCode("push")
  self:addCode(0)
  self:addCode("return")
end


-- generate code from abstract syntax tree
function M.compile(ast)
  Compiler.funcs, Compiler.vars, Compiler.nvars = {}, {}, 0
  for i, func in ipairs(ast) do
    Compiler:codeFunction(func)
  end
  local main = Compiler.funcs["main"]
  if not main then
    error("no function main")
  end
  return main.code
end

--------------------------------------VIRTUAL MACHINE: execute generated code  
  local function create_new_array(n, sizes)
  if n == #sizes then
      return {size=sizes[n]}
  else
    local array = {size=sizes[n]}
    for i = 1, sizes[n] do
      array[#array + 1] = create_new_array(n+1,sizes)
    end
    return array
  end
end

local function create_new_assoc(n, nDims)
  return {assoc = true}
end

local function get_array_keyval(a, n)
  --print("-----------get_array_keyval a=", pt.pt(a), "n",n)
  local i = 0
  for k,v in kpairs(a) do
    --print("k", k, "v", v, "i", i)
    if k =='assoc' then
    else
      i = i + 1
      --print("k2", k, "v2", v, "i2", i, i==n)
      if i == n then return k,v end
    end
  end
end

local function run(code, mem, stack, top, options)
  options = options or {}
  if options.trace then print("\ninterpret") end
  local pc = 1
  local flag_print = false -- to newline first print instruction
  
  -- optionally trace interpretation
  while true do 
    if options.trace then
      io.write(tonumber(top)," -->")
      for i=1, top do
        if type(stack[i]) == "table" then
          io.write("tab:",pt.pt(stack[i]))
        elseif type(stack[i]) == "nil" then
            io.write("nil")
        else
          io.write(stack[i], " ") 
        end
      end
      io.write("  mem: ")
      for i,v in ipairs(mem) do
        if type(v) == "table" then
          io.write("t ")
        else
          io.write(v, " ") 
        end
      end
      if pc < #code then
        io.write("\n",tostring(pc)," ", code[pc]," ", code[pc+1], "\n")
      else
        io.write("\n",tostring(pc)," ", code[pc],"\n")
      end
    end
    
    if code[pc] == "return" then 
      return top
    elseif code[pc] == "call" then 
      pc = pc + 1
      local code = code[pc]
      top = run(code, mem, stack, top, options) -- call run recursively
    elseif code[pc] == "print" then 
      if not flag_print then -- newline first print instruction
        print()
        flag_print = true
      end
      io.write("printing: ")
      if type(stack[top]) == "table" and getmetatable(stack[top]) then 
        -- assume metatable handles it (e.g. zone) TODO: probe mt further for tostring func defn
        print(stack[top]) 
      elseif type(stack[top]) == "table" then
        print(pt.pt(stack[top])) 
      else
        local s = stack[top]
        s = string.gsub(s,[[\n]],"\n") -- undo ignoring nl's
        s = string.gsub(s,[[\t]],"\t") -- undo ignoring nl's
        s = string.gsub(s,[[\"]],'"') -- undo ignoring nl's
        print(s)
      end
      top = top - 1
    elseif code[pc] == "push" then
      pc = pc + 1
      top = top + 1
      stack[top] = code[pc]
    elseif code[pc] == "pop" then
      pc = pc + 1
      top = top - code[pc]
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
    elseif code[pc] == "newarray" then -- {size=sizes[1], {size=sizes[2]... sizes[n]
      local n = stack[top] -- n dims
      local sizes = {}
      for i = 1, n do
        sizes[#sizes + 1] = stack[top - i]
      end
      local newedArray = create_new_array(1, sizes)
      top = top - n
      stack[top] = newedArray
    elseif code[pc] == "newassoc" then -- {assoc=1} TODO: make the key 'assoc' a global table so it is unique
      local nDims = stack[top] 
      local newedAssoc = create_new_assoc(1, nDims)
      stack[top] = newedAssoc
    elseif code[pc] == "getarray" then -- vm mem[array][index] with array, index on stack
      local index = stack[top]
      local array = stack[top - 1]
      if array.assoc then
        --print("-----get assoc array, index =",pt.pt(array), index)
        if type(index) == "number" then --TODO: use more sophisticated form from compiler to distinquish
          -- access assoc for multiple value assignment
          local key, value = get_array_keyval(array, index) 
          --print("mva key, value", key, value)
          if not value then
            print("RUNTIME ERROR: no value for key: " .. tostring(key))
            return 0
          end
          stack[top - 1] = value
        else
          -- access assoc by key
          --print("array[index]=", array[index])
          local value = array[index]
          if not value then
            print("RUNTIME ERROR: no value for key: " .. tostring(index))
            return 0
          end
          stack[top - 1] = value
        end
      else
        --print("-----get array=",pt.pt(array))
        -- index array
        if not (1 <= index and index <= array.size) then
          print("RUNTIME ERROR: getarray out of bounds: index = " .. tostring(index) .. ",  bounds = 1 to " .. tostring(array.size))
          return 0
        else
          stack[top - 1] = array[index]
        end
      end
      top = top -1 
    elseif code[pc] == "setarray" then -- mem[var2num][index] = expr from stack expr, index, table
      local value = stack[top]
      local index = stack[top - 1]
      local array = stack[top - 2]
      top = top - 3
      if array.assoc then
        --print("-------set assoc array,index,value=",pt.pt(array), index, value )
        array[index] = value
        --print("array", pt.pt(array))
      else
        if array.size < index then
          print("RUNTIME ERROR: setarray out of bounds: index = " .. tostring(index) .. ",  bounds = 1 to " .. tostring(array.size))
          return 0
        end
        array[index] = value
      end
    elseif code[pc] == "jmp" then 
      --print("--------jmp pc=", pc, "code[pc+1]=",code[pc+1])
      --print(pc + code[pc+1])
      pc = pc + code[pc+1] -- unconditional relative jump
    elseif code[pc] == "jmpZ" then 
      pc = pc + 1
      if stack[top] == 0 or stack[top] == nil then -- conditional
        pc = pc + code[pc] -- relative jump
      end
      top = top - 1 -- take condition off stack
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
    pc = pc + 1
  end
end

--------------------------------------USER: program in new language, run compiler on it, interpret it
local function pl(cases, options)
  local ok, result
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
      --ok, result = pcall(function ()
      --  return M.compile(ast)
      --end)
      result = M.compile(ast)
      ok = true
      if ok then
        local code = result
        -- optionally trace code
        if options.trace then print("\ncode"); print(pt.pt(code)) end
        
        -- interpret code
        local stack, mem = {}, {}
        --ok, result = pcall(function ( )
        --  return  run(code, mem, stack, 0, options)
        --end)
        ok = true
        run(code, mem, stack, 0, options)
        if ok then
          result = stack[1]
          io.write("\n--->")
          --print("getmetatable", getmetatable(result))
          if getmetatable(result)  then
            print(result) -- assume mt has tostring
          elseif type(result) == "table" then
            print(pt.pt(result))
          else
            print(tostring(result))
          end
          io.write("\n")
        else
          io.write("RUNTIME ERROR: ")
          local pos_last_colon = #result - string.find(string.reverse(result),":")
          print(string.sub(result,pos_last_colon+3,#result))
        end
      else
          io.write("COMPILER ERROR: ")
          local pos_last_colon = #result - string.find(string.reverse(result),":")
          print(string.sub(result,pos_last_colon+3,#result))
      end
      
    end
  end
end

print"\nFunctionality Users Need pl -- aka FUN\n"
print("NEW cases")

function CASES() end -- for IDE outline view
local optTrace, n1, n2 = false, 1, 10
cases = {
[[
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
[[
# so, now let's try some indirect recursion
function definex () {x = 45}
function foo();
function bar() {
  if 42 < x {
    @ "x = {}"  <-  x;  # ooo.. interpolation
    foo()
  } else {
    @ "x = {} {} of {}"  <-  x + 0! -1  <-  "\n\t\tis the end"  <-  "i" | "t";
    return x
  }
}
function foo( ) {
  x = x - 1;
  return bar()
}
function main ( ) {
  definex();
  return bar()
}]]
,
[[
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
[[
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
  unless not not not a ~= a {return a};
  return "Really?"
}]]
,
[[
function foo() {return 99}
function main() {return foo()}]]
,
[[
function foo();
function foo() {return 99}
function foo() {return 42}
function main() {return foo()}]]
,
[[
function foo() {return 42}
function main() {return fo()}]]
,
[[
function foo();
function main() {return foo()}]]
,
[[
function inc() {return 42}
function main() {return inc()}]]
,
[[
function foo() {return 42}
function main() {return foo}]]
,
[[
function main() {
  if 2 < 1 {
    return 5
  } elseif 0 {
    return 7
  } elseif 0 {
    return 9
  } else {
    if 0 {return 99} elseif not 1 < 9 < 3 {return 999}
  }
}
]]
,
[[
function main ( ) {
  a = new[3];
  a[1] = 5;
  a[2] = 6;
  a[3] = 7;
  x,y,z = a;
  @ "{}, {}, and {}" <- x <- y <- z;
  return x:z
}]]
,
}

pl(take(cases,n1, n2),{trace=optTrace})

--[=[
print("\nREGRESSION TESTS")
tests = {
--TODO: surround these in function main() {   ... test }
[[
x=1; y="s";
@ ("Here are {} interpolation{} \n with {} expressions"  <-  12 / 3! + x  <-  y <- "v" | "arious") | ", and a bit more." 
]],
[[@ "str"]]
,
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
return a]]
,
[[
a = new[10];
a[9] = 4;
a[10] = a[9];
@ a[10];
@ a;
return a]]
,
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
]]
,
"return y",
"de =dec dec 1; return de #----- unary ops",
"de =-dec dec 1; return de",
"de =dec -dec 1; return de",
"de =dec - dec 1; return de",
"de =dec dec 1; return = de" 
,
[[
    # the next three lines are fine
    x = 1; #{ this, but not the # embedded comments
    is a block comment spanning #{nesting allowed of course} #, multiple lines }#
    @ (x -3<2);   
    @ ((not 3<-1);  # but this line has a syntax error
    return y
]]
,
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
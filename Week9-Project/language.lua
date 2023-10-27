-- lua 5.4 required (table.move used in base.take)
local lpeg = require"lpeg"
local pt = require"pt" -- misc functions
local lfs = require"lfs" --luafilesystem (used to designate output.txt file for results)
local base = require"base" -- misc functions
local take, gut_table, split, kpairs, tozone, factorial = 
      base.take, base.gut_table, base.split, base.kpairs, base.tozone, base.factorial
local log = print -- to dinstinquish intent i.e. logging/debugging vs normal printing

local M = {} -- module

-- some IDE outline views highlight non local functions; in any case CAPS standout; thus the following hack
function FRONT_END() end  --------------------------------------FRONT END: parse input and make ast

--------- GLOBAL TO PARSER VARIABLES 
local syntaxErr = false   -- to supress superfluous reporting
local ast_vars = {}       -- vars by key
local ast_funcs = {}      -- funcs by key
local fwd_funcs = {}      -- fwd decl'd funcs by key
local fwd_funcs_body = {} -- fwd decl'd funcs by key
local p_maxmatch = 1      -- max position reached by lpeg
local pp = 1              -- for saving lpeg position p
local capturedVar = ""    -- for saving ID for subsequent inspection
local input = ""          -- for syntax error reporting 
-- for calling builtin functions
local builtins = {arraylen = true, assockeys = true, assocvalues = true} 
-- builtin functions .'s assumed e.g. array.len
--TODO: expand this to a standard library framework

local function isbuiltin(id)
  local fpos = id:find(".")
  local nnp = id:gsub("%.","") -- name no period
  --log("fpos=",fpos, "nnp=", nnp)
  if 0 < fpos and builtins[nnp] then
    return true
  end
  return false
end

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
  io.write("\n\nSYNTAX ERROR (pos "..tostring(pmax-1).." of "..tostring(#case).."):\n")
  local num = string.format("%03d",nLine)
  io.write(num," ",
    string.sub(case, range[1], range[2]-1), "\n",
    string.rep(" ", 3 + pmax - range[1]), "^\n"   -- put ^ under msg at syntax err position
  )
  io.write(msg.."\n")
  --log("lines_ranges", pt.pt(lines_range))
  syntaxErr = true
end

-- COMMENTS
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
        M.report_syntax_error(s, p, "block comment closing '}#' not found\n")
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
local spaces = lpeg.space^1 -- used for when space is required

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

-- concat 'expr's with pieces of string
local function foldInterp2(lst)-- string with optional expressions embedded, optional external expressions
  --log("\nlst=", pt.pt(lst))
  if lst[1] == "" then table.remove(lst,1) end
  if lst[#lst] == "" then table.remove(lst) end
  --log("\nlst=", pt.pt(lst))
  
  local tofold = {}
  for i = 1, #lst do
    if type(lst[i]) == "table" then
      table.insert(tofold, lst[i][1])
      if i < #lst then table.insert(tofold, "~~") end
    elseif 0 < #lst[i]  then
      table.insert(tofold, {tag = "string", val = lst[i]})
      if i < #lst then table.insert(tofold, "~~") end
    end
  end
  --log("tofold", pt.pt(tofold))
  
  return foldBin(tofold)
end

-- transform string with interpolation expressions 
-- e.g. "A {} with {} interpolations." <- "string" <- 1 + 1 ---> A string with 2 interpolations."
--
local function foldInterp(lst)-- string with optional expressions embedded, optional external expressions
  -- lower ast to a simple concat binOp and call foldBin on it
  --log("\nlst=", pt.pt(lst))
  local s = lst[1].val -- string to interpolate with remaining expressions
  --log("s=", s)
  
  -- captures to match with expressions
  local NO = (1-lpeg.P"{}")^0
  --local NC = (1-lpeg.P"}")^0
  --local captures = lpeg.Ct((NO * lpeg.P"{" * lpeg.C(NC) * lpeg.P"}")^0)
  local captures = lpeg.Ct((NO * lpeg.C(lpeg.P"{}") )^0 )
  captures = captures:match(s)
  --log("captures", pt.pt(captures))
  
  -- just return the string if it has no captures; any exprs get ignored
  if #captures == 0 then
    return {tag = "string", val = s}
  end
  
  -- expressions to interpolate
  local strings = lpeg.Ct((lpeg.C(NO) * P"{}" * lpeg.C(NO))^0)
  --local strings = lpeg.Ct((lpeg.C(NO) * lpeg.P"{" * NC * lpeg.P"}" * lpeg.C(NO) )^0)
  
 
  local exprs = {}  --TODO: enable interpolating off an indexed expression e.g. a a[1] i.e. 1 and 2 dim array or a zone
                    -- use similar technique to getting them as done for multiple assignment
 for i = 3, #lst, 2 do
    exprs[#exprs + 1] = lst[i]
  end
  
  -- pad exprs
  if #exprs < #captures then
    for i = 1, #captures - #exprs do
      exprs[#exprs + 1] = exprs[#exprs]
    end
  end
  --log("exprs", pt.pt(exprs))

  -- strings between the captures for concatenating with exprs 
  strings = strings:match(s)
  --log("\nstrings", pt.pt(strings))
  
  -- ingore surplus exprs
  if  #captures < #exprs then
    for i = 1, #exprs - #captures  do
      table.remove(exprs)
    end
  end
  --log("exprs", pt.pt(exprs))
  

  s = {}
  for i = 2, #strings, 2 do
    if strings[i-1] ~='' then 
      s[#s+1] = {tag="string", val=strings[i-1]}
      s[#s+1] = "~~"
    end
    s[#s+1] = exprs[i/2] 
    s[#s+1] = "~~"
    s[#s+1] = {tag="string", val=strings[i]}
    s[#s+1] = "~~"
  end
  table.remove(s,#s)
  --log("\ns", pt.pt(s))
  return foldBin(s)
end

-- only allow a triplet; syntax error otherwise
-- pick two of three
local function composeRel(lst)
  local tree = { }
  if 5 < #lst then
    if not syntaxErr then M.report_syntax_error(input, p_maxmatch - 3, "mathematical relations: only 2 relations allowed") end
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

-- compose for in terms of a block with local vars and a while loop (i.e. no compiler or vm changes required)
local function composeFor(lst)
  --log("For lst=", pt.pt(lst))
  -- intervar, from, to [, step], body
  local itervar = lst[1]
  local from = lst[2]
  local to = lst[3]
  local step, body
    if #lst == 5 then
    step = lst[4]
    body = lst[5]
  else
    step = {tag = "number", val = 1}
    body = lst[4]
  end
  --log("itervar", pt.pt(itervar))
  --log("from", pt.pt(from))
  --log("to", pt.pt(to))
  --log("step", pt.pt(step))
  --log("body", pt.pt(body))
  
  local tree = {
    tag = 'block',
    body = {
      tag = 'seq',
      stmt1 = {
          tag = 'local',
          name = itervar.var,
          init = from },
      stmt2 = {
        tag = 'seq',
        stmt1 = {
          tag = 'local',
          name = 'to',
          init =  to },
        stmt2 = {
          tag = 'seq',
          stmt1 = {
            tag = 'local',
            name = 'step',
            init ={
              tag = 'preop',
              op = '-',
              e1 = step } },
          stmt2 = {
            tag = 'while_',
            cond = {
              tag = 'binop',
              e1 = {
                tag = 'binop',
                e1 = {
                  tag = 'binop',
                  e1 = {
                    tag = 'number',
                    val = 0},
                  op = '<',
                  e2 = step},
                op = 'and',
                e2 = {
                  tag = 'binop',
                  e1 =   itervar,
                  op = '<=',
                  e2 = to}},
              op = 'or',
              e2 = {
                tag = 'binop',
                e1 = {
                  tag = 'binop',
                  e1 = step,
                  op = '<',
                  e2 = {
                    tag = 'number',
                    val = 0}},
                op = 'and',
                e2 = {
                  tag = 'binop',
                  e1 = to,
                  op = '<=',
                  e2 = itervar}}},
            body = {
              tag = 'block',
              body = {
                tag = 'seq',
                stmt1 = body,
                stmt2 = {
                  tag = 'assgn',
                  lhs = itervar,
                  expr = {
                    tag = 'binop',
                    e1 = itervar,
                    op = '+',
                    e2 = step
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  --log("tree=", pt.pt(tree))
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
local ID = object + identifier

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
  ["var"] = true, 
  ["for"] = true, 
}    

local OP = P("(") * space      -- open parenthesis      
local CP = P(")") * space      -- close parenthesis
local OB = P("{") * space      -- open braces      
local CB = P("}") * space      -- close braces

-- operators from lowest to highest priority
-- lpeg.S("+-~") * -#(lpeg.B("<")) * -#(lpeg.P(">"))  -- enable <- <| <+ -> |> +> etc operators
local opA = lpeg.C( (lpeg.P"+" * -#(lpeg.S"+>") * -#(lpeg.B"<") )
            + (lpeg.P"-" * -#(lpeg.S"->") * -#(lpeg.B"<") )
            + (lpeg.P"~" * -#(lpeg.S"~>") * -#(lpeg.B"<") ) -- find e.g. s1 ~ s2
            ) * space  --binary left associative add, subract, concat (for zone)
local opI = lpeg.C(lpeg.P("<-")) * space   --binary left associative string interpolation
local opM = lpeg.C(lpeg.S("*/%")) * space  --binary left assocociative multiply, divide, remainder
local opZ = lpeg.C(P(":")) * space         --binary left associative zone
local opE = lpeg.C(lpeg.S("^")) * space    --binary right associative exponent
local opS = lpeg.C(lpeg.S("!")) * space    --unuary suffix left associative factorial
local opP = lpeg.C(lpeg.S("-+") + "~" * -#(lpeg.P"=") ) * space  --unuary right associative, spaces optional           
local opPs= lpeg.C(                        --unuary right associative, spaces mandatory (could be an ID otherwise)
            P"dec"       
                + "inc"
                + "not"
            ) * spaces 

-- "<", ">", ">=", "<=", "==", and "!="
local opR = lpeg.C(
                P"<="
                + ">=" 
                + "~=" 
                + "==" 
                + "<" * -#(lpeg.S("+-|"))         -- enable <- <| <+ operators
                + ">" * -#(lpeg.B(lpeg.S("+-|"))) -- enable -> |> +> operators
              ) * space

local opLand = lpeg.C(P"and") * #(-alpha_numeric) * space
local opLor = lpeg.C(P"or") * #(-alpha_numeric) * space

local function Rw(word) 
  assert(keywords[word]) -- defensive pgming to ensure we've added it to keywords
  return word * -#(alpha_numeric) * space 
end


function PROCESS_CAPTURES() end -- for IDE outline view

--[[
take {'a',  {val = 'k1', tag = 'string'}, {val = 'k2', tag = 'string'  } ...}  associative arrays
                                         {val = 1, tag = "number"}             arrays
return
{tag = 'indexed',
 array = {
   tag = 'indexed'},
   array = {
     var = 'a',
     tag = 'variable'},
   index = {
     val = 'k1',
     tag = 'string'} }, ... nesting
 index =   {
   val = 'j2';
   tag = 'string'} } 
]]
local function foldIndexed_use(lst)
  --log("----foldIndexed_use lst=", pt.pt(lst))
  local tree = {tag="variable", var = lst[1]} 
  -- array e.g. {tag = 'variable', var = 'x'} TODO: capture this in grammar capture_index
  for i = 2, #lst do
    tree = {tag = "indexed", array = tree, index = lst[i] }
  end
  --log("--tree=", pt.pt(tree))
  return tree
end

local function foldNew_assgn(lst) 
  --log("foldNew_assgn lst", pt.pt(lst))
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

local function foldNew_local_assgn(lst) 
  --log("-------foldNew_local_assgn lst", pt.pt(lst))
  -- distinquish associative arrays
  if lst[#lst] == "assoc" then
    -- create an associative arrary
    
    -- make this a local where expr is the init
    local tree = {tag = "local", name =  lst[1].var, init = {tag = 'new', assoc = {tag = "number", val = #lst-1} }}
    
    return tree
  else
    local sizes = { } 
    for i = #lst, 2, -1 do
      sizes[#sizes + 1] = lst[i]
    end
    
    local tree = {tag = "local", name =  lst[1].var, init = {tag = 'new', sizes = sizes}}
    --log("--tree=", pt.pt(tree))
    return tree
  end
end


local function foldIndexed_assgn(lst) --array, i1, i2, ...iN, expr
  --log("----foldIndexed_assgn lst", pt.pt(lst))
  local lhs = {tag="variable", var = lst[1]} 
  for i = 2, #lst - 1 do
    lhs = {tag = "indexed", array = lhs, index = lst[i] }
  end
  local tree = {tag = "assgn", expr = lst[#lst], lhs = lhs}
  --log("--foldIndexed_assgn tree=", pt.pt(tree))
  return tree
end

local function var_assgn(ast) 
  --log("----var_assgn lst=", pt.pt(lst))
  local tree = {tag = "assgn", lhs = ast[1], expr = ast[2]}
  return tree
end


local function foldStmts(a)
  local tree = {tag = "seq", stmt1 = a[1], stmt2 = a[2]}
  for i=3,#a do
    tree = {tag = "seq", stmt1 = tree, stmt2=a[i]}
  end
  return tree
end

local function multi_assgn(lst) --used for destructuring arrays
  -- lower to a sequence of indexed assignments
  -- for associative arrays the index number will correspond to the i'th iterated key
  --log("----multi_assgn lst=", pt.pt(lst))
  local array = lst[#lst]
  -- array could be {tag = "variable", var = "a"} for a one dimensional array
  --             or {tag = "indexed", array = {tag = "variable", var = "a"}, index = {tag = "string", val = "key"}} for a 2 dim array
  --             or nested deeper for 3+ dim arrays
  -- we want to wrap either with another level of indexing to get the values for the mulitple variables
  -- i.e. {tag = "indexed", array = array, index = {tag = "number", val = 1}}
  
  -- create local vars for values to assign so overlapping assignments don't interfer, 
  -- thus enabling e.g. a[2], a[1] = a i.e. a swap
  local stmts = {}
  for i = 1, #lst-1 do
    stmts[#stmts + 1] = {tag = "local", 
                             name = "__" .. i, 
                             init = {tag = "indexed", array = array, index = {tag = "number", val = i}} }
  end
  
  -- wrap indexing around array for each  multiple var and assign to temp local vars
  for i = 1 , #lst-1 do
    stmts[#stmts + 1] = {tag = "assgn", 
                             lhs = lst[i], -- variable or indexed  
                             expr = {tag = "variable", var = stmts[i].name } 
                            } 
  end
  --log("\n--stmts=", pt.pt(stmts))
  -- put all the stmts in a folded sequence
  local tree = foldStmts(stmts) 
  --log("\n--tree=", pt.pt(tree))
  return tree
end

-- same as above but with local assignments AND ignoring arrays as locals
local function multi_local_assgn(lst) --used for destructuring arrays
  --log("----multi_local_assgn lst=", pt.pt(lst))
  local array = lst[#lst]
  local stmts = {}
  for i = 1, #lst-1 do
    stmts[#stmts + 1] = {tag = "local", 
                             name = "__" .. i, 
                             init = {tag = "indexed", array = array, index = {tag = "number", val = i}} }
  end
  
  -- wrap indexing around array for each  multiple var and assign to temp local vars
  for i = 1 , #lst-1 do
    
    -- just use regular assignment if indexed (local doesn't make sense for arrays), but still allow a mix
    if lst[i].tag == "variable" then      
      stmts[#stmts + 1] = {tag = "local", 
                            name =  lst[i].var, -- variable 
                            init = {tag = "variable", var = stmts[i].name }  
                          } 
    else
      stmts[#stmts + 1] = {tag = "assgn", 
                               lhs = lst[i], -- indexed  
                               expr = {tag = "variable", var = stmts[i].name } 
                              } 
    end
  end
  --log("\n--stmts=", pt.pt(stmts))
  
  local tree = foldStmts(stmts) 
  --log("\n--tree=", pt.pt(tree))
  return tree
end


-- all the functions are captured first and then processed here
local function nodeFuncDecl(lst)
  --log("-----nodeFunctDecl lst=", pt.pt(lst))
  local name = lst[1]
  local params_ = lst[2]
  local params = params_[1] or {}
  local def_params = params_[2] or {}
  --log("---- nodeFuncDecl name ", name)
  --log("--params=", pt.pt(params))
  --log("--def_params=", pt.pt(def_params))
  --log("--fwd_funcs=", pt.pt(fwd_funcs))
  if fwd_funcs[name] == nil then
    fwd_funcs[name] = true
  else
    if not syntaxErr then M.report_syntax_error(input, ast_vars[name], "function '" .. name .. "' has been declared more than once\n")  end
  end

  local tree = {tag = "function", name = name, body = {}, params = params, def_params = def_params}
  --log("--tree", pt.pt(tree))
  return tree
end

-- all the functions are captured first and then processed here
local function nodeFuncDefn(lst)
  --log("-----nodeFuncDefn lst=", pt.pt(lst))
  local name = lst[1]
  local params_ = lst[2]
  local params = params_[1] or {}
  local def_params = params_[2] or {}
  local body = lst[3]
  --log("-----nodeFuncDefn name ", name)
  --log("--params=", pt.pt(params))
  --log("--def_params=", pt.pt(def_params))
  --log("--body=", pt.pt(body))
  --log("ast_funcs", pt.pt(ast_funcs))
  --log("fwd_funcs=", pt.pt(fwd_funcs))
  --log("--1)fwd_funcs_body=", pt.pt(fwd_funcs_body))
  if ast_funcs[name] == nil then
    ast_funcs[name] = true
  else
    if not syntaxErr then M.report_syntax_error(input, ast_vars[name], "function '" .. name .. "' has already been defined\n")  end
  end
  local tree = {tag = "function", name = name, body = body, params = params, def_params = def_params}
  --log("--tree", pt.pt(tree))
  return tree
end


local function isempty(t)
  for k,_ in pairs(t) do
    return false
  end
  return true
end


local function checkbodies(lst)
  --log("lst", pt.pt(lst), p_maxmatch)
  local funcs = {}
  for i, func in ipairs(lst) do
    funcs[func.name] = func.body
  end
  --log("funcs", pt.pt(funcs))
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
local funcDefn = lpeg.V"funcDefn"
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
local shortcctA = lpeg.V"shortcctA"
local shortcctO = lpeg.V"shortcctO"
local shortcctC = lpeg.V"shortcctC"
local interp = lpeg.V"interp"


-- captures
local indexed_capture = lpeg.V"indexed_capture"
local new_capture = lpeg.V"new_capture"
local var_capture = lpeg.V"var_capture"
local local_capture = lpeg.V"local_capture"
local nil_capture = lpeg.V"nil_capture"
local indexed_use = lpeg.V"indexed_use"
local var_use = lpeg.V"var_use"
local func_capture = lpeg.V"func_capture"
local call_capture = lpeg.V"call_capture"
local param_capture = lpeg.V"param_capture"
local params = lpeg.V"params"
local def_param_capture = lpeg.V"def_param_capture"
local args = lpeg.V"args"

local function calc_maxp(s,p) p_maxmatch = math.max(p_maxmatch, p); return true end
    
function GRAMMAR() end -- for IDE outline view

local Ct = lpeg.Ct

-- string pattern
local open1 = lpeg.P('"') 
local open2 = lpeg.P("'") 
local close1 = -lpeg.B("\\") * open1 + open1 * -1 
local close2 = -lpeg.B("\\") * open2 + open2 * -1 
local text1 = lpeg.C((1 - (close1 + "`"))^0) 
local text2 = lpeg.C((1 - (close2 + "`"))^0)
local strpat = ( (open1 * text1 * close1 )
               + (open2 * text2 * close2 ) 
               ) * space

local grammar = P{"prog",
  prog = space * Ct( (funcDecl + funcDefn)^1 ) / checkbodies * -1,
  
  funcDecl = Ct(Rw"function" * func_capture *  T";")  / nodeFuncDecl,
  funcDefn = Ct(Rw"function" * func_capture * block)  / nodeFuncDefn,
  call = call_capture / node("call", "fname", "args"),
  
  stmts = stmt * (T";"^1 * stmts)^-1 / nodeSeq,
  
  stmt = 
        T"{" * T"}" -- empty blocks allowed
      + block
      
      -- local assignments
      + Rw"var" * Ct(new_capture * (T"[" * (interp + lpeg.Cc("assoc") ) * T"]")^1) / foldNew_local_assgn  
      + Rw"var" *  Ct( (Ct(indexed_capture) / foldIndexed_use  + var_capture)  * (T"," * 
                       (Ct(indexed_capture) / foldIndexed_use  + var_capture ))^1 * T"="  * interp) / multi_local_assgn
      + Rw"var" * local_capture * ( (T"=" * interp) + lpeg.Cc("nil") ) / node("local", "name", "init")
      
      + Rw"for" * Ct(var_capture * T"=" * interp * T"," * interp * (T"," * interp)^-1  * block) / composeFor * space 
      + Rw"while" * interp * block / node("while_", "cond", "body")   
      + Rw"unless" * interp * block / node("unless", "cond", "body")   
      + Rw"if" * interp * block * nested_elseifs^-1 / node("if_", "cond", "then_", "else_")
      + Rw"return" * interp / node("return", "expr") 
      + call
      
      -- assignments
      + Ct(new_capture * (T"[" * (interp + lpeg.Cc("assoc") ) * T"]")^1) / foldNew_assgn  
      + Ct( (Ct(indexed_capture) / foldIndexed_use  + var_capture)  * (T"," * 
            (Ct(indexed_capture) / foldIndexed_use  + var_capture ))^1 * T"="  * interp) / multi_assgn
      + Ct(indexed_capture * T"=" * interp ) / foldIndexed_assgn 
      + nil_capture -- GC vars and arrays
      + Ct(var_capture * T"=" * interp) / var_assgn
      
      + T"@" * interp / node("print","expr") -- printing
      + space, 
      
  block = T"{" * stmts * T";"^-1 * T"}" / node("block", "body"), -- extra semicolons allowed
  
  nested_elseifs = Rw"else" * block + Rw"elseif" * interp * block *  nested_elseifs^-1
                  / node("if_", "cond", "then_", "else_"),
                  
  factor =  
        number  / tonumber / node("number","val")
      + open1 * Ct((text1 * lpeg.P"`" * Ct(shortcctC) * lpeg.P"`" * text1)^0) * close1 * space / foldInterp2-- string interpolation 
      + open2 * Ct((text2 * lpeg.P"`" * Ct(shortcctC) * lpeg.P"`" * text2)^0) * close2 * space / foldInterp2-- string interpolation 
      + strpat / node("string", "val") 
      + T"(" * interp * T")" 
      + Ct(indexed_use * (T"[" * interp * T"]")^0) / foldIndexed_use 
      + T"nil"
      + call
      + var_use / node("variable", "var"),
      
  prefix = Ct((opP + opPs)^1  * factor )  / foldPre + factor,                                  
  zone = Ct(prefix * (opZ  * prefix)^0 ) / foldBin  * space , 
  suffix = Ct(zone * opS^1) / foldSuf * space + zone,
  expo = Ct(suffix * (opE * suffix)^0 ) / foldRight  * space,  
  term = Ct(expo * (opM * expo)^0 ) / foldBin  * space, 
  expr = Ct(term * (opA  * term)^0 ) / foldBin  * space, 
  relation = Ct(expr * (opR  * expr)^0 ) / composeRel  * space ,  
  notty =  Ct(opPs^1 * relation) / foldPre  + relation,
  shortcctA = lpeg.Ct(notty * (opLand * notty)^0 ) / foldBin  * space,  
  shortcctO = lpeg.Ct(shortcctA * (opLor * shortcctA)^0 ) / foldBin  * space,  
  shortcctC = lpeg.Ct(shortcctO * (lpeg.C("~~") * space * shortcctO)^0 ) / foldBin  * space,  
  interp = Ct(strpat / node("string", "val")  * (opI  * shortcctC)^1 ) / foldInterp  * space + shortcctC, 
  space = (lpeg.space + block_comment + comment)^0 * calc_maxp,
  
  indexed_capture = #(ID * space * T"[") *      -- is indexed var
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
    * space * ( T"[" * interp * T"]" )^1,

 
 new_capture = #(ID * space * T"="  * T"new") * -- is new array 
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
    / node("variable", "var") * space * T"=" * T"new",
 
 var_capture = #(ID * space * (T"=" + T",") ) * -- is var 
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
    / node("variable", "var") * space,
 
 local_capture = #(ID * space) *  -- is local var 
    function(s,p) pp = p; return true end *  -- matchtime save ID position in pp
    lpeg.C(ID) *                             -- capture ID
    function(s,p)                            -- matchtime syntax/semantic checks and register var                                    
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
    * space ,
    
 nil_capture = #(ID * space * T"=" * T"nil") *  -- is nil 
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
   * space * T"=" * T"nil",
 
 
 indexed_use = #(ID * space * T"[") * 
    function(s,p) pp = p; return true end * 
    lpeg.C(ID) *
    function(s,p)
      local id = string.sub(s, pp, p-1)
      --log("id=", "'"..id.."'")
      if id == "new" then return false end
      --log("id2=", id)
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
  * space ,
  
 var_use = #(ID) * 
    function(s,p) pp = p; return true end * lpeg.C(ID) *
    function(s,p)
      local id = string.sub(s, pp, p-1)
      if id == "new" then return false end
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
        --log("not defined var_use id=", id)
        return false 
      else
        return true
      end
    end
  * space ,
 
 --[[       log("----func_capture fwd_funcs=", id,  pt.pt(fwd_funcs))
        log("--fwd_funcs_body=", pt.pt(fwd_funcs_body),"\n")
        if fwd_funcs[id] == nil then          
          fwd_funcs[id] = 1
        elseif fwd_funcs[id] == 1 and #fwd_funcs_body[id] == 0 then
          if not syntaxErr then M.report_syntax_error(s, p, "function ".."'"..id.."' is already forward declared\n") end       
          return false
        elseif 2 < fwd_funcs[id]  then
          if not syntaxErr then M.report_syntax_error(s, p, "function ".."'"..id.."' is already defined\n") end       
          return false
        else
          fwd_funcs[id] = fwd_funcs[id] + 1
]]
  
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
        ast_vars[id] = p
        return true
      end
    end
  * space * T"(" * params * T")",
  
  params = Ct( ( Ct( param_capture * (T"," * param_capture)^0) * -- reg params
                Ct( (T"," * def_param_capture)^0 ) )^-1    -- params with default expresssions
              ),
  param_capture = #(ID) *     -- is name 
    function(s,p) pp = p; return true end * -- matchtime save ID position in pp
    lpeg.C(ID) * space * -#(T"=") *                            -- capture parameter name
    function(s,p)                           -- matchtime syntax/semantic checks and register var                                    
      local id = string.sub(s, pp, p - 1)
      local isReserved = keywords[id]
      if isReserved then
        if not syntaxErr then M.report_syntax_error(s, p, "cannot use reserved word ".."'"..id.."' as a function name\n") end
        return false 
      else
        ast_vars[id] = true -- TODO: scope these with function and remove at end of nodeFunction
        --log("param id=", id)
        return true
      end
    end * space ,
  
  def_param_capture = #(ID) *     -- is name 
    function(s,p) pp = p; return true end * -- matchtime save ID position in pp
    lpeg.C(ID) *                          -- capture parameter name
    function(s,p)                           -- matchtime syntax/semantic checks and register var                                    
      local id = string.sub(s, pp, p - 1)
      local isReserved = keywords[id]
      if isReserved then
        if not syntaxErr then M.report_syntax_error(s, p, "cannot use reserved word ".."'"..id.."' as a function name\n") end
        return false 
      else
        ast_vars[id] = true -- TODO: scope these with function and remove at end of nodeFunction
        --log("param id=", id)
        return true
      end
    end * space * T"=" * interp / node("default", "name", "expr"),

  call_capture = -#(Rw"while") * #(ID * space * T"(") *     -- is name 
    function(s,p) pp = p; return true end * -- matchtime save ID position in pp
    lpeg.C(ID) *                            -- capture ID aka name
    function(s,p)                           -- matchtime syntax/semantic checks and register var                                    
      local id = string.sub(s, pp, p - 1)
      local isReserved = keywords[id]
      --log("isbuiltin?", isbuiltin(id))
      if isReserved then
        if not syntaxErr then M.report_syntax_error(s, p, "cannot use reserved word ".."'"..id.."' as a function call\n") end
        return false 
      elseif (not ast_vars[id]) and (not isbuiltin(id)) then
        if not syntaxErr then M.report_syntax_error(s, p, "function '"..id.."' hasn't been defined\n") end
        return false 
      else
        --log("call func id=", id)
        return true
      end
    end
  * space * T"(" * args * T")",
  
  args = Ct( ( interp * (T"," * interp)^0 )^-1 ),
  
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

local Compiler = {funcs = {}, vars = {}, nvars = 0, locals = {}}

--------- [op] = instruction codes

-- left associative binary 
local binOps = {
  ["<"] = "lt", [">"] = "gt", ["<="] = "le", [">="] = "ge", ["=="] = "eq", ["~="] = "ne", 
  ["+"] = "add", ["-"] = "sub", ["~"] = "str", ["~~"] = "con",
  ["*"] = "mul", ["/"] = "div", ["%"] = "rem", 
  [":"] = "zone",
  ["<-"] = "interp",
}

-- right associative binary
local rightOps = {
  ["^"] = "exp",
}

-- prefix, space optional or mandatory; not is lowest priority, the others prefix priority
local preOps = {
  ["~"] = "len",
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

-- local values are placed on stack; the functions finds the idx to them

-- when looking for a default param we shouldn't look in local variables because by not giving it we want the default


function Compiler:findLocalIndex(name)
  -- default lookups should not see local vars
  --log("-----findLocalIndexname", name)
  
    -- is name a local var? look here first to shadow params and globals
    local locs = self.locals
    --log("name", "'"..name.."'", #locs, "locs=", pt.pt(locs))
    for i = #locs, 1, -1 do
      if locs[i] == name then
        return i -- on stack after base of locals so idx forward
      end
    end
  -- is name a regular param? look here next to shadow globals
  local params = self.params
  local def_params = self.def_params
  local n_tot_params = #params + #def_params
  --log("--is a param?")
  --log("--name=", name)
  --log("--params=", pt.pt(params))
  --log("--def_params=", pt.pt(def_params))
  for i = #params, 1, -1 do
    if params[i] == name then
      return -(n_tot_params - i) -- on stack before base of locals so idx backward
    end
  end
  
  -- is name a default param? look here next
  for i = #params + #def_params, #params + 1, -1 do
    if def_params[i-#params].name == name then
      return -(n_tot_params - i) -- on stack before base of locals so idx backward
    end
  end
  return nil
end

  -- instead of looking stuff up in func, look at builtin objects
  -- do the correct params checks
  -- instead of addCode('call') addCode('callBuiltin')
  -- instead of addCode(func.code) addCode(ast.fname)

function Compiler:codeBuiltin(fname, ast)
  --log("coding for calling builtin function "..fname)
  -- check and code params per codeCall
  local params = ast.args
  local def_params = {}
  local args = ast.args
  --log("-----------coding call for ", ast.fname)
  --log("--params", pt.pt(params))
  --log("--def_params", pt.pt(def_params))
  --log("--args", pt.pt(args))
  --log("-----------checking args for ", ast.fname)
  --log("func", pt.pt(func))
  --log("ast", pt.pt(ast))
  --log("args", pt.pt(args))
  if #args  < #params or #params + #def_params < #args then
    error("wrong number of arguments calling function " .. ast.fname)
  end
  
  for i = 1, #args do
    -- the associated lookups for these should be shadowed by local vars
    self.default_lookup_flag = false
    self:codeExp(args[i]) 
  end
  
  --log("def_params", pt.pt(def_params))
  if 0 < #def_params then
    for i = #args, #def_params do
      -- the associated lookups for these should NOT be shadowed by local vars i.e. assigning to local var should not change default param
      self.default_lookup_flag = true
      --log("def_params[i].expr", i, pt.pt(def_params[i].expr))
      self:codeExp(def_params[i].expr) 
    end
  end

  self:addCode("lib") -- a libary function
  self:addCode(fname)
end


function Compiler:codeCall(ast)
  --capture calls to builtin functions 
  if isbuiltin(ast.fname) then
    Compiler:codeBuiltin(ast.fname, ast)
  else
    local func = self.funcs[ast.fname]
    
    if not func then -- TODO: the following checks will be done in the parser and thus will be redundant here: delete post testing
      error("undefined function " .. ast.fname)
    end
    local params = func.params
    local def_params = func.def_params
    local args = ast.args
    --log("-----------coding call for ", ast.fname)
    --log("--params", pt.pt(params))
    --log("--def_params", pt.pt(def_params))
    --log("--args", pt.pt(args))
    --log("-----------checking args for ", ast.fname)
    --log("func", pt.pt(func))
    --log("ast", pt.pt(ast))
    --log("args", pt.pt(args))
    if #args  < #params or #params + #def_params < #args then
      error("wrong number of arguments calling function " .. ast.fname)
    end
    
    for i = 1, #args do
      -- the associated lookups for these should be shadowed by local vars
      self.default_lookup_flag = false
      self:codeExp(args[i]) 
    end
    
    --log("def_params", pt.pt(def_params))
    if 0 < #def_params then
      for i = #args, #def_params do
        -- the associated lookups for these should NOT be shadowed by local vars i.e. assigning to local var should not change default param
        self.default_lookup_flag = true
        --log("def_params[i].expr", i, pt.pt(def_params[i].expr))
        self:codeExp(def_params[i].expr) 
      end
    end
    self:addCode("call")
    self:addCode(func.code)
    --log("--code so far")
    --log(pt.pt(self.code))
  end
end

function Compiler:currentPC()
  return #self.code 
end

-- jump forward needs to be fixed
function Compiler:codeJmpF(op)
  self:addCode(op)
  self:addCode(0) -- to be updated later using the pc returned here together with and a following pc
  return self:currentPC() 
end

-- fixes forward jumps (backward ones are coded by codeJmpB above)
function Compiler:fixJmp2here(jmp)
  -- jmp is previous pc
  local diff = self:currentPC() - jmp
  -- when this occurs pc is looking at the jump amount so the diff is one less (but it gets added in vm after elseifs of instructions
  self.code[jmp] = diff
end

-- jump back can be directly implemented
function Compiler:codeJmpB(op, iPC)
  self:addCode(op)
  local diff = iPC - self:currentPC() 
  self:addCode(diff) -- jump backward to pc == iPC
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
      local jmp = self:codeJmpF("jmpA")     -- (jmpS) leaves cond on stack
      --self:addCode("pop") -- if we do e2 it means we don't need the cond so pop it
      --self:addCode(1)        -- doing the jump will pop the cond
      self:codeExp(ast.e2) 
      self:fixJmp2here(jmp)  
    elseif ast.op == "or" then
      -- code short cct or; i.e. if e1=true leave e1 on stack (and skip e2); else leave e2 result on stack 
      self:codeExp(ast.e1)
      local jmp = self:codeJmpF("jmpO")     
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
    --log("exp variable lookup ast.var=", ast.var)
    local idx = self:findLocalIndex(ast.var)
    --log("idx=", idx)
    --log("ast.var=", "'"..ast.var.."'", "idx=",idx, "locals=", pt.pt(self.locals))
    if idx then
      self:addCode("loadL")
      self:addCode(idx)
    else
      self:addCode("load")
      self:addCode(self:var2num(ast.var))
    end
  
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
  --log("****codeAssgn variable lookup ast.lhs=", pt.pt(lhs))
  if lhs.tag == "variable" then
      self:codeExp(ast.expr)
      local idx = self:findLocalIndex(lhs.var)
      --log("idx", idx)
      if idx then
        self:addCode("storeL")
        self:addCode(idx)
      else
        self:addCode("store")
        self:addCode(self:var2num(lhs.var))
      end
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
  local oldlevel = #self.locals
  Compiler.oldlevel = oldlevel
  self:codeStmt(ast.body)
  -- remove locals associated with this block
  local diff = #self.locals - oldlevel
  --log("-----codeBock self.locals=", pt.pt(self.locals),"\ndiff=", diff)
  if 0 < diff then
    for i=1, diff do table.remove(self.locals) end
    self:addCode("pop")
    self:addCode(diff)
  end
end

function Compiler:findLocalinScope(name, level)
    for i = #self.locals, level, -1 do
      if self.locals[i] == name then
        return i
      end
    end
    return nil
end


-- generate code from ast statements
function Compiler:codeStmt(ast)
  if ast == nil or ast.tag == nil  then
    -- empty stmt, or syntaxErr do nothing
  
  elseif ast.tag == "assgn" then
    self:codeAssgn(ast)
  
  -- create a new local var for defaults not given (because they are invisible)
  elseif ast.tag == "local" then 
    --log("======local ast=", pt.pt(ast))
    -- Compiler.locals variables are ordered by creation
    -- we use that to reverse search for a local variable to load it
    -- but in creating one nothing may be there, or one in a higher scope may be
    -- we want to allow the higher scope one, but not one in the immediate scope
    -- distinquish the current scope by ... using oldLevel to limit the search    
    -- to extend this to include not allowing var n where n is a parameter the parameters need to be
    -- added to the stack per the overview below the base, with local vars above the base
    -- check that var is not being declared again    
    -- TODO: advance this check to the parser (along with a type system)
    --       so precise position is available to make a better error msg
    if self:findLocalinScope(ast.name, self.oldlevel+1) then
      local msg = "local var ".. ast.name .." was already defined"
      print(msg)
      error(msg) -- if pcall is enabled in compiler this will not stop processing (may be disabled for debugging)
    else
      --log("--self.def_params=", pt.pt( self.def_params))
      local init = ast.init
      if init =="nil" then
        init = {tag = "number", val = 0} -- TODO: emit var ast.name not initialized warning
      end
      self:codeExp(init)
      self.locals[#self.locals + 1] = ast.name
      --log("self.locals", pt.pt(self.locals))
      --log("code so far:", pt.pt(self.code))
    end
  
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
    local jmp = self:codeJmpF("jmpZ") -- (jmpZ) conditionally jump past body code and jmp
    self:codeStmt(ast.body)
    self:codeJmpB("jmp", iPC)        -- unconditionally jump back to top of loop (i.e. to start of ast.cond to execute it again)
    self:fixJmp2here(jmp)
    
  elseif ast.tag == "unless" then            
    local iPC = self:currentPC()
    self:codeExp(ast.cond)
    self:addCode("not") -- negate cond
    local jmp = self:codeJmpF("jmpZ")    -- (jmpZ) conditional jump
    self:codeStmt(ast.body)
    self:fixJmp2here(jmp)
    
  elseif ast.tag == "if_" then
    self:codeExp(ast.cond)
    local jmp = self:codeJmpF("jmpZ")     
    self:codeStmt(ast.then_) 
    if ast.else_ == nil then
      self:fixJmp2here(jmp)                
    else                                -- if then_ block executes, we need to skip the else_ block 
      local jmp2 = self:codeJmpF("jmp")  -- (jmp) unconditional jump forward
      self:fixJmp2here(jmp)             -- fix conditional jump to skip uncondional jump
      self:codeStmt(ast.else_)
      self:fixJmp2here(jmp2)
    end
  
  elseif ast.tag == "return" then
    self:codeExp(ast.expr)
    self:addCode("return")
    --log("---- ret self #locals,params,def_params", #self.locals, #self.params, #self.def_params)
    --log("--self.locals=", pt.pt(self.locals))
    self:addCode(#self.locals + #self.params + #self.def_params)
  
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
  local name = ast.name
  -- if a non 'main' func exists update its code 
  if name ~= "main" and self.funcs[name] then
    local func = self.funcs[name]
    -- remove func's previous code and params
    --log("gutting func", name, pt.pt(self.funcs[name]))
    gut_table(func.code) 
    if #ast.params ~= #func.params then
      error("not the same number of parameters as fwd definition for function "..name)
    end
    --log("func", name, pt.pt(self.funcs[name]))
    -- point code to the func's code
    self.code = func.code
  else
      -- associate new func with new code table and point to that
      local code = {}
      self.funcs[name] = {code = code, params = ast.params, def_params = ast.def_params} 
      self.code = code
  end
  self.params = ast.params
  self.def_params = ast.def_params
  self:codeStmt(ast.body) 
  --log("coded func", name, pt.pt(self.funcs[name]))
  --log("compiler funcs=====", pt.pt(self.funcs))
  -- make all functions, aka code generated, end with a return stmt
  self:addCode("push")
  self:addCode(0)
  self:addCode("return")
  self:addCode(#self.locals + #self.params + #self.def_params)
end


-- generate code from abstract syntax tree
function M.compile(ast)
  Compiler.code, Compiler.funcs, Compiler.vars, Compiler.nvars = {}, {}, {}, 0
  Compiler.locals, Compiler.params, Compiler.def_params = {}, {}, {}
  -- NOTE: interestingly we have tag = "function" but we just assume it in the compiler; good for viewing the AST tho
  for i, func in ipairs(ast) do 
    Compiler:codeFunction(func)
  end
  local main = Compiler.funcs["main"]
  if not main then
    error("no function main")
  end
  if 0 < #Compiler.funcs["main"].params then
    error("main cannot have parameters")
  end
  return main.code
end

--------------------------------------VIRTUAL MACHINE: execute generated code and output results
io.output(lfs.currentdir().."\\output.txt")

local function create_new_array(n, sizes)
  --og("n", n, " sizes=", pt.pt(sizes))
  if n == #sizes then
      return {size=sizes[n]}
  else
    local array = {size=sizes[n]}
    for i = 1, sizes[n] do
      array[#array + 1] = create_new_array(n + 1, sizes)
    end
    return array
  end
end

local function create_new_assoc(n, nDims)
  return {assoc = true}
end

local function get_array_keyval(a, n)
  --log("-----------get_array_keyval a=", pt.pt(a), "n",n)
  local i = 0
  for k,v in kpairs(a) do
    --log("k", k, "v", v, "i", i)
    if k =='assoc' then
    else
      i = i + 1
      --log("k2", k, "v2", v, "i2", i, i==n)
      if i == n then return k,v end
    end
  end
end


local flag_print = false -- to newline first print instruction

local function run(code, mem, stack, top, options)
  options = options or {}
  if options.trace_interp then io.write("\ninterpret\n") end
  
  local pc = 1
  local base = top -- of local variables
  
  -- optionally trace interpretation
  while true do 
    if options.trace_interp then
      io.write(tonumber(top)," -->")
      for i=1, top do
        if type(stack[i]) == "table" then
          io.write("tab:",pt.pt(stack[i])," ")
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
        if type(code[pc])=='table' or type(code[pc+1])=='table' then
          io.write("\n",tostring(pc)," ", pt.pt(code[pc]), "\n")
        else  
          io.write("\n",tostring(pc)," ", code[pc]," ", code[pc+1], "\n")
        end
      else
        io.write("\n",tostring(pc)," ", code[pc],"\n")
      end
    end
    
    if code[pc] == "return" then 
      local n = code[pc + 1] -- number of active local variables
      stack[top - n] = stack[top] --TODO: could do GC when stack shrinks such as here
      return top - n
    elseif code[pc] == "call" then 
      pc = pc + 1
      local code = code[pc]
      top = run(code, mem, stack, top, options) -- call run recursively
    elseif code[pc] == "lib" then 
      pc = pc + 1
      local fname = code[pc]
      --log("executing library function " .. fname)
      stack[top] = stack[top].size
    elseif code[pc] == "print" then 
      if not flag_print then -- newline first print instruction
        io.write("\n")
        flag_print = true
      end
      if not options.supress_prnt_wrap then io.write("printing: ") end
      -- assume metatable handles it (e.g. zone)
      if type(stack[top]) == "table" and getmetatable(stack[top]) then -- TODO: probe mt further for tostring func defn
        io.write(tostring(stack[top]),"\n") 
      elseif type(stack[top]) == "table" then
        io.write(pt.pt(stack[top]),"\n") 
      else
        local s = stack[top]
        s = string.gsub(s,[[\n]],"\n") -- undo ignoring nl's
        s = string.gsub(s,[[\t]],"\t") -- undo ignoring nl's
        s = string.gsub(s,[[\"]],'"')  -- undo ignoring nl's TODO: better pattern matching and replacement of arbitrary esc sequences
        io.write(s)
        if not options.supress_prnt_wrap then io.write("\n") end
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
    elseif code[pc] == "loadL" then 
      pc = pc + 1
      local id = code[pc]
      top = top + 1
      --log("id", id, "base" ,base , stack[base + id])
      stack[top] = stack[base + id]
    elseif code[pc] == "store" then 
      pc = pc + 1
      local id = code[pc]
      mem[id] = stack[top]
      top = top - 1
    elseif code[pc] == "storeL" then 
      pc = pc + 1
      local id = code[pc]
      stack[base+ id] = stack[top]
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
        --log("-----get assoc array, index =",pt.pt(array), index)
        if type(index) == "number" then --TODO: use more sophisticated form from compiler to distinquish
          -- access assoc for multiple value assignment
          local key, value = get_array_keyval(array, index) 
          --log("mva key, value", key, value)
          if not value then
            io.write("RUNTIME ERROR: no value for key: " .. tostring(key),"\n")
            return 0
          end
          stack[top - 1] = value
        else
          -- access assoc by key
          --log("array[index]=", array[index])
          local value = array[index]
          if not value then
            io.write("RUNTIME ERROR: no value for key: " .. tostring(index),"\n")
            return 0
          end
          stack[top - 1] = value
        end
      else
        --log("-----get array=",pt.pt(array))
        -- index array
        if not (1 <= index and index <= array.size) then
          io.write("RUNTIME ERROR: getarray out of bounds: index = " .. tostring(index) .. ",  bounds = 1 to " .. tostring(array.size), "\n")
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
        --log("-------set assoc array,index,value=",pt.pt(array), index, value )
        array[index] = value
        --log("array", pt.pt(array))
      else
        if array.size < index then
          io.write("RUNTIME ERROR: setarray out of bounds: index = " .. tostring(index) .. ",  bounds = 1 to " .. tostring(array.size),"\n")
          return 0
        end
        array[index] = value
      end
    -- JUMPS NB pc = pc + 1 after elseifs so we jump by 1 less than we would otherwise
    elseif code[pc] == "jmp" then 
      local diff = code[pc+1] -- relative amount
      if 0 < diff then diff = diff + 1 end
      pc = pc + diff  -- unconditional jump 
    elseif code[pc] == "jmpZ" then 
      pc = pc + 1
      if stack[top] == 0 or stack[top] == nil then -- conditional
        pc = pc + code[pc]  
      end
      top = top - 1 -- take condition off stack
    elseif code[pc] == "jmpA" then  
      pc = pc + 1 
      -- if not jumping take the cond off the stack since the next expr will give leave it
      if stack[top] == 0 or stack[top] == nil then -- conditional
        pc = pc + code[pc]  
      else
        top = top - 1 -- take condition off stack
      end
    elseif code[pc] == "jmpO" then  --
      pc = pc + 1
      -- if not jumping take the cond off the stack since the next expr will give leave it
      if not ( stack[top] == 0 or stack[top] == nil) then -- conditional
        pc = pc + code[pc] 
      else
        top = top - 1 -- take condition off stack
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
    -- unuary ~ ---> string length
    elseif code[pc] == "len" then 
      stack[top] = #stack[top]
    -- binary ~ ---> string operations
    elseif code[pc] == "str" then 
      local s = stack[top - 1]
      local z = stack[top]
      -- substr (including trim)
      if type(z) == "table" then
        local n1, n2 = z[1], z[2]
        local space = lpeg.S' \t\n\v\f\r'
        local nospace = 1 - space
        local ptrim = space^0 * lpeg.C((space^0 * nospace^1)^0)       
        if n1 == 0 then
          s = ptrim:match(s)
          --log ("s=".. string.sub(s, 1, n2))
        end
        if n2 == 0 then
          s = string.reverse(ptrim:match(string.reverse(s)))
          n2 = -1
          --log ("s=".. string.sub(s, n1, n2))
        end
        stack[top - 1] = string.sub(s, n1, n2)
      -- find
      elseif type(s) == "string" and type(z) == "string" then
        local n1, n2 = string.find(z,s)
        stack[top - 1] = tozone({n1, n2}) 
      else
        stack[top - 1] = s
      end
      top = top - 1
     elseif code[pc] == "con" then
       local x = stack[top]
       -- allow types such as zone with a metatable to convert to string themselves
       if type(x) == "table" and not getmetatable(x) then
         x = pt.pt(x)
       end
       local y = stack[top-1]
       if type(y) == "table" and not getmetatable(y) then
         y = pt.pt(y)
       end
      stack[top-1] = tostring(y) .. tostring(x) 
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
function M.pl(cases, options)
  local ok, result
  options = options or {trace = false}
  for _, grouped_case in ipairs(cases) do  
    local i = grouped_case[1]
    local case = grouped_case[2]
    local num = string.format("%03d",i)
    io.write("\nPROGRAM ----------------------------- ", num, " ------------------------------\n")
    local lines = split(case,"\n")
    for i, line in ipairs(lines) do
        local num = string.format("%03d",i)
        io.write(num, " ", line, "\n")
    end
    io.write("\n") -- separate from code listing
        
    ast, p_maxmatch, syntaxErr = M.parse(case, options)
    
    if not ast then
      if not syntaxErr then M.report_syntax_error(case, p_maxmatch, "", options) end
    -- report any SYNTAX ERROR
    elseif p_maxmatch < #case then 
      if not syntaxErr then M.report_syntax_error(case, p_maxmatch, "", options) end
    elseif not syntaxErr then
      -- optionally trace ast
      if options.trace_ast then io.write("\nast\n"); io.write(pt.pt(ast).."\n\n") end
      --ok, result = pcall(function ()
      --  return M.compile(ast)
      --end)
      result = M.compile(ast)
      ok = true
      if ok then
        local code = result
        -- optionally trace code
        if options.trace_code then io.write("\ncode\n"); io.write(pt.pt(code),"\n") end
        
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
          --log("getmetatable", getmetatable(result))
          if getmetatable(result)  then
            io.write(tostring(result),"\n") -- assume mt has tostring
          elseif type(result) == "table" then
            io.write(pt.pt(result),"\n")
          else
            io.write(tostring(result),"\n")
          end
          io.write("\n")
        else
          io.write("RUNTIME ERROR: ")
          local pos_last_colon = #result - string.find(string.reverse(result),":")
          io.write(string.sub(result,pos_last_colon+3,#result),"\n")
        end
      else
          io.write("COMPILER ERROR: ")
          local pos_last_colon = #result - string.find(string.reverse(result),":")
          io.write(string.sub(result,pos_last_colon+3,#result),"\n")
      end

    end
  end
end

return M
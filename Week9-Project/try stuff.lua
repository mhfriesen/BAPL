local lpeg = require"lpeg"
--print("lpeg.version", lpeg.version())
lpeg.locale(lpeg)
local pt = require"pt" 
local base = require"base" 

builtins = {arraylen = true}

local function isbuiltin(id)
  local fpos = id:find(".")
  local nnp = id:gsub("%.","")
  print("fpos=",fpos, "nnp=", nnp)
  if 0 < fpos and builtins[nnp] then
    return true
  end
  return false
end

print(isbuiltin("array.len"))



--[[
s = [ [
my
func
] ]

local function indent(s)
  local s2 = "  "
  for i = 1, #s do
    local sub = string.sub(s,i,i)
    if string.byte(sub) == 10 then
      sub = sub .. "  "
    end
    s2 = s2 .. sub
  end
  return s2
end
print(indent(s))



function fold(a)
  local tree = {tag = "seq", stmt1 = a[1], stmt2 = a[2]}
  for i=3,#a do
    tree = {tag = "seq", stmt1 = tree, stmt2=a[i]}
  end
  return tree
end

print(pt.pt(fold({"assgn1","assgn2","assgn3"})) )
print()


    return {tag = "seq", stmt1 = stmt1, stmt2 = stmt2}

-- convert captured lists into a tree, left associatively
local function foldBin(lst)
  local tree = lst[1]
  for i=2, #lst, 2 do
    tree = { tag = "binop", e1 = tree, op = lst[i], e2 = lst[i+1] }
  end
  return tree
end

{
expr =   {
  expr =     {
    expr = 1;
    a = 1;    };
  a = 2;  };
a = 3;}

function nest(a, n)
  if n < 2 then
    return {a[1]} 
  end
  return {a[n], nest(a, n-1)}
end

print(pt.pt(nest({"assgn1","assgn2","assgn3"},3)) )
]]

--[[{
{assign1, assign2}, assign3}
expr =   {
  expr =     {
    expr = 1;
    a = 1;    };
  a = 2;  };
a = 3;}
]]

--[[
{{stmt3, stmt2}, stmt1}
]]


--[=[
--function foo(x);
function bar(x)
  if 42 < x then
   return foo(x)
  else 
    return x
  end
end

function foo(x) 
  x = x - 1
  return bar(x)
end

function main ( )
  return bar(45)
end

print(main())



--[[

-- convert captured lists into a tree, left associatively
local function foldBin(lst)
  local tree = lst[1]
  for i=2, #lst, 2 do
    tree = { tag = "binop", e1 = tree, op = lst[i], e2 = lst[i+1] }
  end
  return tree
end


      expr =         {
        tag = 'binop';
        e2 =           {
          tag = 'number';
          val = 6;          };
        op = '+';
        e1 =           {
          tag = 'binop';
          e2 =             {
            tag = 'number';
            val = 5;            };
          op = '+';
          e1 =             {
            tag = 'number';
            val = 4;            };          };        };

]]



















--[=[
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

a  =  {
  x = 5,
  y = 6, 
  assoc = true
  }

for k,v in kpairs(a) do
  print(k , v)
end

local function isempty(t)
  for k,_ in pairs(t) do
    return false
  end
  return true
end

print(isempty{a="1"})

local function split(s, sep)
  sep = lpeg.P(sep)
  local elem = lpeg.C((1 - sep)^0)
  local p = lpeg.Ct(elem * (sep * elem)^0)   -- make a table capture
  return lpeg.match(p, s)
end

-- a, b, c = myArray
function a()
for i=1, 10 do
  if i==4 then return i end
end
end

print(a())


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

a = create_new_array(1,{4,3,2})
print(pt.pt(a))
a[4][2][2] = "test"
print(pt.pt(a))
print(pt.pt(a[4][3][2]))

set = {key = true, for_ = true}
print(pt.pt(set))




str= "x=1"
p = lpeg.P(1) * #(lpeg.P("=")) * "=" * "1"
print (p:match(str))

local alnum = lpeg.alnum^1

local alpha = lpeg.R("az") + lpeg.R("AZ")
local numeric = lpeg.R("09")

--local keywords = {"if", "else", "elseif", "end"}
local keywords = {"if", "elseif", "else", "end"}
local keyword = lpeg.P(false)
for _, kw in ipairs(keywords) do
 keyword = keyword + kw
end

--keyword = keyword * I"A" * -alnum 
keyword = keyword * -alnum 

print(lpeg.match(keyword, "else") )
print(lpeg.match(keyword, "elseif"))
print(lpeg.match(keyword, "else1"))


-- make these assertions to pass
assert(lpeg.match(keyword, "else") == 5)
assert(lpeg.match(keyword, "elseif") == 7)
assert(lpeg.match(keyword, "else1") == nil)

print()

local open = lpeg.P('"')
--local close = open
--local close = -lpeg.B("\\") * I"A" * open * I"B"
local close = -lpeg.B("\\") * open + open * -1
local p = open * lpeg.C((1 - close)^0) * close

print(p:match([["a\"b"]]))
print(p:match([["a\\"]]))
print(p:match([["xyz\\"]]))
  
-- make these assertions to pass
assert([[a\"b]] == p:match([["a\"b"]]))
assert([[a\\]] == p:match([["a\\"]]))
assert([[xyz\\]] == p:match([["xyz\\"]]))






pat = #(-alnum) * P(1) * lpeg.C(P"and" + "or") * #(-alnum) * P(1)

print(pat:match(" and"))

local function prepend(t,t2)
  local r = {}
  table.move(t,1,#t,#r+1,r)
  table.move(t2,1,#t2,#r+1,r)
  return r
end

print(pt.pt(prepend({{1,2}},{{"a", "b"}})))


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

a = node("assgn", "id", "expr")
print(pt.pt( a("x","(1+2)") ) )


local function I(msg)
  return function(s,p) print(msg.." p="..tostring(p)) return true end
end

function P(str) 
  return function(s,p)
  -- at p match str in s
  if string.sub(s, p, p + #str - 1) == str then
    p = p + #str
    return p
  end
  return false
  end
end

pat = lpeg.P("") * P"ab" * I"A" * lpeg.P(1)^0 * -1

print(pat:match("abcd") )
--]=]
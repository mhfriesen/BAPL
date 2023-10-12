local lpeg = require"lpeg"
local lfs = require"lfs" --luafilesystem
dofile(lfs.currentdir().."\\pt.lua") --table to string
dofile(lfs.currentdir().."\\test.lua") --table to string
nl = "\n"

function exactly(pat, n) 
  local p = lpeg.P(0) * pat -- enable p to be a string
  return #(p^n) * p^-n * -p -- check(at least 6 p)  * at most 6 p * not p
end 

local space = lpeg.S(" \t\n")^0
local digits = lpeg.R("09")^1
local hexdigits = lpeg.R("09","AF","af")
local dot = lpeg.P(".")
local hex = "0" * lpeg.S("xX") * exactly(hexdigits,2) 
local decimal = digits * dot * digits +
                digits * dot  + 
                dot * digits +
                digits
local scientific = decimal *  lpeg.S("eE") * ((exactly("-",1) * digits) + digits)
local number = scientific + hex + decimal * space  

--print(exactly("-",2):match"--")
--print(decimal:match"10.")
print(scientific:match"1e10")
test("numbers",number,{
    "10",
    "10.",
    ".10",
    "1.10",
    "1e10",
    "1e-10",
    "0xAf",
})

--print(number:match"1e10")

--[[
function join(t1,t2)
  return table.move(t2,1,#t2,#t1+1, t1)
end

function fold(t, f)
  local acc = t[1]
  for i=2,#t do
    acc = f(acc,t[i])
  end
  return acc
end

local function strings_longest(strings)
  return string.len(fold(join({""},strings), function(s1,s2) return string.len(s1)<string.len(s2) and s2 or s1 end))
end

function string_repeated(s,n)
  local str = ""
  for i=1,n do str = str..s end
  return str
end


--print(fold({1,2,10},function(a,b) return a + b end))
--print(strings_longest({"aa","abc","a"}))
--print(strings_longest({"aass","abc","a"}))
---print(strings_longest({"a","abc"}))
--print(string_repeated(" ",10)..".")
---print(tostring(nil))


--]]

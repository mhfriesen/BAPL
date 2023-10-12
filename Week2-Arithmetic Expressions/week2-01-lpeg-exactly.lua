local lpeg = require"lpeg"
local lfs = require"lfs" --luafilesystem
dofile(lfs.currentdir().."\\pt.lua") --table to string
dofile(lfs.currentdir().."\\test.lua") --table to string
loc = lpeg.locale()



--[[

print"UPTO  6 hex digits"
d = lpeg.R("09","AF")
p = d^-6 * -d
print(p:match("123456"))
print(p:match("12345"))
print(p:match("1234567"))

print"\nEXACTLY 6 digits"

function exactly(p,n)
  local r = lpeg.P(0) -- enable p to be a lteral
  for i=1,n do
    r = r * p
  end
  return r * (lpeg.P(0) -p)
end


print"literal"
lit = d * d * d * d * d * d * -d
print(lit:match"1234AB")
print(lit:match"1234A")
print(lit:match"1234ABC")

print"generative lpeg"
print(exactly(d,6):match"1234AB")
print(exactly(d,6):match"1234A")
print(exactly(d,6):match"1234ABC")

print("generative lpeg p can be a string")
print(exactly(" ",2):match"  1234ABC")
print(exactly(" ",2):match" 1234ABC")
print(exactly(" ",2):match"   1234ABC")

print"consise lpeg"
p = #(d^6) * d^-6 * -d -- check(at least 6 p)  * at most 6 p * not p
print(p:match("123456"))
print(p:match("12345"))
print(p:match("1234567"))

print"generative consise lpeg"

function exact(p, n) 
  local r= lpeg.P(0) * p -- enable p to be a string
  return #(r^n) * r^-n * -r 
end 

p = exact(d,6)
print(p:match("123456"))
print(p:match("12345"))
print(p:match("1234567"))

print"generative consise lpeg; p can be a string"
print(exact(" ",2):match("  1234567"))
print(exact("  ",1):match("  1234567"))


-]]

print"\nmanually execute stack machine program"
--[[
push 4
push 2
push 24
push 21
sub
mult
add"
--]]
print((4 +2) * (24-21),"(4 +2) * (24-21)")

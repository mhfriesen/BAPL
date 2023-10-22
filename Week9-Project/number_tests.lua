local lfs = require"lfs" --luafilesystem
--dofile(lfs.currentdir().."\\pt.lua") --table to string

print(lfs.currentdir().."\\tests.lua")

local lines = {}
local file = io.open(lfs.currentdir().."\\tests.lua", "r")
for line in file:lines() do
  table.insert(lines, line)
end


-- number programs for the end after the 54 primordial ones
local n = 55
newlines = {}
for i = #lines, 1, -1 do
  local nl, r =  lines[i]:gsub("^[%d]+,", n..",")
  if 0 < r then
    n = n + 1
  end
  newlines[#newlines + 1] = nl
end
file:close()
 
-- reverse again for original order
file = io.open(lfs.currentdir().."\\tests.lua", "r+")
for i = #newlines, 1, -1 do
  file:write(newlines[i].."\n")
end
file:close()
  

